module Cardano.Indexer.Reactor
  ( runReactor,
  ) where

import Cardano.Indexer.Config (App, ReactorActions)
import Cardano.Indexer.Config qualified as Cfg

import Control.Concurrent.STM.TBQueue (readTBQueue)

runReactor :: App ()
runReactor = loop =<< asks Cfg.cfgEvents
  where
    loop queue = do
      msg <- liftIO . atomically . readTBQueue . Cfg.unReactorQueue $ queue
      res <- runReactorAction msg
      if msg == Cfg.Finish
        then pure res
        else loop queue

runReactorAction :: ReactorActions -> App ()
runReactorAction Cfg.WriteBlock = writeBlock
runReactorAction Cfg.RollbackBlock = rollbackBlock
runReactorAction Cfg.Finish = pure ()

writeBlock :: App ()
writeBlock = pure ()

rollbackBlock :: App ()
rollbackBlock = pure ()
