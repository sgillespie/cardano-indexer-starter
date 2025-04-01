module Cardano.Indexer
  ( Config.AppT (..),
    Config.App,
    Config.Config (..),
    Config.runAppT,
    runIndexer,
  ) where

import Cardano.Indexer.Config (App, Config)
import Cardano.Indexer.Config qualified as Config

runIndexerCli :: IO ()
runIndexerCli = _

runIndexer :: Config -> IO ()
runIndexer = Config.runAppT indexer

indexer :: App ()
indexer = pure ()
