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
    AppError (..),
    DatabaseDir,
    NodeConfigFile (..),
    StandardBlock,
    TopologyConfigFile (..),
  )
import Cardano.Indexer.Config qualified as Config

import Cardano.Api (BlockType (..), Protocol (..))
import Cardano.BM.Trace (stdoutTrace)
import Cardano.Indexer.ChainSync (runNodeClient)
import Cardano.Node.Configuration.POM
  ( NodeConfiguration (..),
    PartialNodeConfiguration (..),
    defaultPartialNodeConfiguration,
    makeNodeConfiguration,
    parseNodeConfigurationFP,
  )
import Cardano.Node.Protocol (SomeConsensusProtocol, mkConsensusProtocol)
import Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import Cardano.Node.Types (ConfigYamlFilePath (..), TopologyFile (..))
import Control.Monad.Trans.Except (except)
import Ouroboros.Consensus.Node (NodeDatabasePaths (..), ProtocolInfo)

runIndexer :: Options -> IO ()
runIndexer CLI.Options{..} = do
  protoInfo <- loadProtocolInfo optNodeConfig optTopologyConfig optDatabaseDir

  let
    config =
      Config.Config
        { cfgMagic = optNetworkMagic,
          cfgSocketPath = optSocketPath,
          cfgProtocolInfo = protoInfo,
          cfgTrace = stdoutTrace
        }

  Config.runAppT indexer config

indexer :: App ()
indexer = runNodeClient

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
              { pncConfigFile = Last configYamlFile,
                pncTopologyFile = Last topologyFile,
                pncDatabaseFile = Last dbFile
              }
           )
    cfg = makeNodeConfiguration partialCfg'

  pure $ first (NodeConfigError . toText) cfg

mkConsensusProtocol' :: NodeConfiguration -> ExceptT AppError IO SomeConsensusProtocol
mkConsensusProtocol' cfg =
  withExceptT (NodeConfigError . textShow) $
    mkConsensusProtocol
      (ncProtocolConfig cfg)
      (Just $ ncProtocolFiles cfg)

mkCardanoBlockType
  :: SomeConsensusProtocol
  -> Either AppError (ProtocolInfo StandardBlock)
mkCardanoBlockType (SomeConsensusProtocol CardanoBlockType runP) =
  Right $ fst (protocolInfo @IO runP)
mkCardanoBlockType (SomeConsensusProtocol blockTy _) =
  Left $ NodeConfigError $ "Unexpected block type: " <> textShow blockTy
