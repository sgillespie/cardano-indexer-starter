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

import Cardano.Api (BlockType (..), Protocol (..))
import Cardano.Indexer.CLI (Options)
import Cardano.Indexer.CLI qualified as CLI
import Cardano.Indexer.Config (App, AppError (..), NodeConfigFile (..), StandardBlock)
import Cardano.Indexer.Config qualified as Config
import Cardano.Node.Configuration.POM
  ( NodeConfiguration (..),
    makeNodeConfiguration,
    parseNodeConfigurationFP,
  )
import Cardano.Node.Protocol (SomeConsensusProtocol, mkConsensusProtocol)
import Cardano.Node.Protocol.Types (SomeConsensusProtocol (..))
import Cardano.Node.Types (ConfigYamlFilePath (..))
import Control.Monad.Trans.Except (except)
import Ouroboros.Consensus.Node (ProtocolInfo)

runIndexer :: Options -> IO ()
runIndexer CLI.Options{..} = do
  protoInfo <- loadProtocolInfo optNodeConfig

  let
    config =
      Config.Config
        { cfgMagic = optNetworkMagic,
          cfgSocketPath = optSocketPath,
          cfgProtocolInfo = protoInfo
        }

  Config.runAppT indexer config

indexer :: App ()
indexer = pure ()

loadProtocolInfo :: NodeConfigFile -> IO (ProtocolInfo StandardBlock)
loadProtocolInfo (NodeConfigFile file) = do
  cfgRes <-
    runExceptT $ do
      cfg <- loadNodeConfig file
      someProto <- mkConsensusProtocol' cfg
      except $ mkCardanoBlockType someProto

  either throwIO pure cfgRes

loadNodeConfig :: FilePath -> ExceptT AppError IO NodeConfiguration
loadNodeConfig file = ExceptT $ do
  cfg <- loadNodeConfig' configYamlFile
  pure $ first (NodeConfigError . toText) cfg
  where
    loadNodeConfig' = fmap makeNodeConfiguration . parseNodeConfigurationFP
    configYamlFile = Just (ConfigYamlFilePath file)

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
