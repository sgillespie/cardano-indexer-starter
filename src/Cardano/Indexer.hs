module Cardano.Indexer
  ( Config.AppT (..),
    Config.App,
    Config.Config (..),
    Config.runAppT,
    CLI.Options (..),
    CLI.NetworkMagic (..),
    CLI.parseOptions,
    runIndexer,
  ) where

import Cardano.Indexer.CLI (Options)
import Cardano.Indexer.CLI qualified as CLI
import Cardano.Indexer.Config
  ( App,
    AppError,
    DatabaseDir,
    NodeConfigFile,
    StandardBlock,
    TopologyConfigFile,
  )
import Cardano.Indexer.Config qualified as Config
import Cardano.Indexer.Reactor (runReactor)

import Cardano.Api (BlockType (..), Protocol (..))
import Cardano.BM.Trace (appendName, logError, stdoutTrace)
import Cardano.Indexer.ChainSync (runNodeClient)
import Cardano.Node.Configuration.POM
  ( NodeConfiguration,
    defaultPartialNodeConfiguration,
    makeNodeConfiguration,
    parseNodeConfigurationFP,
  )
import Cardano.Node.Configuration.POM qualified as POM
import Cardano.Node.Protocol (SomeConsensusProtocol, mkConsensusProtocol)
import Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import Cardano.Node.Types (ConfigYamlFilePath (..), TopologyFile (..))
import Control.Concurrent.Class.MonadSTM.Strict (newTBQueueIO, newTVarIO, writeTBQueue)
import Control.Monad.Trans.Except (except)
import Ouroboros.Consensus.Node (NodeDatabasePaths (..), ProtocolInfo (..))
import UnliftIO.Async qualified as UnliftIO
import UnliftIO.Exception qualified as UnliftIO

runIndexer :: Options -> IO ()
runIndexer CLI.Options{..} = do
  protoInfo <- loadProtocolInfo optNodeConfig optTopologyConfig optDatabaseDir
  ledgerState <- newTVarIO $ Config.LedgerState (pInfoInitLedger protoInfo)
  queue <- newTBQueueIO 50 -- arbitrary
  let
    config =
      Config.Config
        { cfgMagic = optNetworkMagic,
          cfgSocketPath = optSocketPath,
          cfgProtocolInfo = protoInfo,
          cfgTrace = stdoutTrace,
          cfgEvents = Config.ReactorQueue queue,
          cfgLedgerState = ledgerState
        }

  Config.runAppT indexer config

indexer :: App ()
indexer = do
  withFinalizerAsync runReactor shutdownReactor $ \_ ->
    UnliftIO.withAsync runNodeClient $ \nodeClient ->
      UnliftIO.wait nodeClient
  where
    withFinalizerAsync action finalizer inner = UnliftIO.withAsync action $ \async' ->
      inner async' `UnliftIO.finally` finalizer async'

    shutdownReactor reactor = do
      queue <- asks (Config.unReactorQueue . Config.cfgEvents)
      tracer <- asks Config.cfgTrace

      res <- UnliftIO.try $ do
        liftIO $
          atomically (writeTBQueue queue Config.Finish)
        UnliftIO.wait reactor
      case res of
        Right () -> pure ()
        Left (err :: SomeException) -> liftIO $ do
          logError (appendName "Indexer" tracer) (toText $ displayException err)

loadProtocolInfo
  :: NodeConfigFile
  -> TopologyConfigFile
  -> DatabaseDir
  -> IO (ProtocolInfo StandardBlock)
loadProtocolInfo cfgFile topoFile dbDir = do
  cfgRes <-
    runExceptT $ do
      cfg <- loadNodeConfig cfgFile topoFile dbDir
      someProto <- mkConsensusProtocol' cfg
      except $ mkCardanoBlockType someProto

  either throwIO pure cfgRes

loadNodeConfig
  :: NodeConfigFile
  -> TopologyConfigFile
  -> DatabaseDir
  -> ExceptT AppError IO NodeConfiguration
loadNodeConfig cfgFile topoFile dbDir = ExceptT $ do
  let
    configYamlFile = Just (ConfigYamlFilePath $ Config.unNodeConfigFile cfgFile)
    topologyFile = Just (TopologyFile $ Config.unTopologyConfigFile topoFile)
    dbFile = Just (OnePathForAllDbs $ Config.unDatabaseDir dbDir)

  partialCfg <- parseNodeConfigurationFP configYamlFile

  let
    partialCfg' =
      defaultPartialNodeConfiguration
        <> ( partialCfg
              { POM.pncConfigFile = Last configYamlFile,
                POM.pncTopologyFile = Last topologyFile,
                POM.pncDatabaseFile = Last dbFile
              }
           )
    cfg = makeNodeConfiguration partialCfg'

  pure $ first (Config.NodeConfigError . toText) cfg

mkConsensusProtocol' :: NodeConfiguration -> ExceptT AppError IO SomeConsensusProtocol
mkConsensusProtocol' cfg =
  withExceptT (Config.NodeConfigError . textShow) $
    mkConsensusProtocol
      (POM.ncProtocolConfig cfg)
      (Just $ POM.ncProtocolFiles cfg)

mkCardanoBlockType
  :: SomeConsensusProtocol
  -> Either AppError (ProtocolInfo StandardBlock)
mkCardanoBlockType (SomeConsensusProtocol CardanoBlockType runP) =
  Right $ fst (protocolInfo @IO runP)
mkCardanoBlockType (SomeConsensusProtocol blockTy _) =
  Left $ Config.NodeConfigError $ "Unexpected block type: " <> textShow blockTy
