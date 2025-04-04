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
import Cardano.Indexer.Config (App)
import Cardano.Indexer.Config qualified as Config

runIndexer :: Options -> IO ()
runIndexer CLI.Options{..} = Config.runAppT indexer config
  where
    config = Config.Config
      { cfgMagic = optNetworkMagic,
        cfgSocketPath = optSocketPath,
        cfgProtocolInfo = _  -- TODO[sgillespie]
      }


indexer :: App ()
indexer = pure ()
