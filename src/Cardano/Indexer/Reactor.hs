module Cardano.Indexer.Reactor
  ( runReactor,
    applyBlock,
  ) where

import Cardano.Indexer.Config
  ( App,
    LedgerState,
    ReactorActions,
    StandardBlock,
    StandardPoint,
    StandardServerTip,
  )
import Cardano.Indexer.Config qualified as Cfg

import Cardano.BM.Trace (appendName, logError)
import Cardano.Indexer.Ledger (readLedgerSnapshot, writeLedgerSnapshot)
import Control.Concurrent.Class.MonadSTM.Strict qualified as STM
import Ouroboros.Consensus.Block qualified as Block
import Ouroboros.Consensus.HeaderValidation (headerStateBlockNo, headerStatePoint)
import Ouroboros.Consensus.Ledger.Abstract (tickThenReapply)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import Ouroboros.Consensus.Node (ProtocolInfo (..))
import Ouroboros.Network.Block (Tip (..), getTipBlockNo)
import Ouroboros.Network.Point (withOrigin)
import UnliftIO (atomically, hFlush)
import Prelude hiding (atomically)

runReactor :: App ()
runReactor = loop =<< asks Cfg.cfgEvents
  where
    loop queue = do
      msg <- atomically $ STM.readTBQueue (Cfg.unReactorQueue queue)
      res <- runReactorAction msg
      case msg of
        Cfg.Finish -> pure res
        _ -> loop queue

runReactorAction :: ReactorActions -> App ()
runReactorAction (Cfg.Init whenFinished) = init whenFinished
runReactorAction (Cfg.WriteBlock serverTip block) = writeBlock serverTip block
runReactorAction Cfg.Finish = finish
runReactorAction (Cfg.RollbackBlock serverTip point whenFinished) = rollbackBlock serverTip point whenFinished

init :: (StandardPoint -> IO ()) -> App ()
init whenFinished = do
  tracer <- asks Cfg.cfgTrace
  ledgerStateVar <- asks Cfg.cfgLedgerState

  liftIO $
    logError (appendName "Reactor" tracer) "Received initialization signal"

  -- Read ledger state from filesystem
  maybeLedgerState <- readLedgerSnapshot
  -- Set ledger to previous state if it couldn't be read
  ledgerState <- parseLedgerState maybeLedgerState ledgerStateVar
  -- Update the application state
  atomically (STM.writeTVar ledgerStateVar ledgerState)

  -- Look up the client point from ledger state
  let
    startPoint = headerStatePoint . headerState . Cfg.unLedgerState $ ledgerState

  liftIO $ do
    whenFinished startPoint
  where
    parseLedgerState (Just ledger) _ = pure ledger
    parseLedgerState Nothing ledgerStateVar = liftIO (STM.readTVarIO ledgerStateVar)

writeBlock :: StandardServerTip -> StandardBlock -> App ()
writeBlock serverTip block = do
  -- Add current block to ledger state
  void $ applyBlock block
  -- Print status to console
  reportBlock serverTip block

applyBlock :: StandardBlock -> App LedgerState
applyBlock block = do
  protoInfo <- asks Cfg.cfgProtocolInfo
  ledger <- asks Cfg.cfgLedgerState
  atomically $ do
    ledgerState <- STM.readTVar ledger

    let
      ledgerState' = applyLedgerState protoInfo block ledgerState
    STM.writeTVar ledger ledgerState'
    pure ledgerState'

applyLedgerState
  :: ProtocolInfo StandardBlock
  -> StandardBlock
  -> LedgerState
  -> LedgerState
applyLedgerState protoInfo block = mapLedgerState (tickThenReapply ledgerCfg block)
  where
    ledgerCfg = ExtLedgerCfg (pInfoConfig protoInfo)
    mapLedgerState f (Cfg.LedgerState s) = Cfg.LedgerState (f s)

reportBlock :: (MonadIO io) => StandardServerTip -> StandardBlock -> io ()
reportBlock (Cfg.ServerTip serverTip) clientBlock = do
  let
    -- Get the block number
    (Block.BlockNo blockNo') = Block.blockNo clientBlock
    -- Tip block number
    serverBlockNo =
      case serverTip of
        TipGenesis -> "Genesis"
        Tip _ _ (Block.BlockNo serverBlockNo') -> textShow serverBlockNo'
    -- Are we close the tip?
    upToDate = blockNo' + 500 >= withOrigin 0 Block.unBlockNo (getTipBlockNo serverTip)
  when (blockNo' `mod` 500 == 0 || upToDate) $ do
    putStr $
      "\rReceived block: "
        <> textShow blockNo'
        <> " of "
        <> serverBlockNo
        <> "          " -- Add a some extra space to avoid garbled output
    hFlush stdout

rollbackBlock
  :: StandardServerTip
  -> StandardPoint
  -> (StandardServerTip -> StandardPoint -> IO ())
  -> App ()
rollbackBlock serverTip point whenFinished = do
  protoInfo <- asks Cfg.cfgProtocolInfo
  ledgerStateVar <- asks Cfg.cfgLedgerState

  -- Roll back ledger state
  newLedgerState <-
    atomically $ do
      ledgerState <- STM.readTVar ledgerStateVar

      -- Lookup the ledger state point
      let
        statePoint = headerStatePoint . headerState . Cfg.unLedgerState $ ledgerState
        newInitState = Cfg.LedgerState (pInfoInitLedger protoInfo)

      -- If we're already at the rollback point, we don't need to rollback any
      -- further
      if point == statePoint
        then pure ledgerState
        else do
          STM.writeTVar ledgerStateVar newInitState
          pure newInitState

  reportRollback serverTip newLedgerState
  liftIO $ whenFinished serverTip point

reportRollback :: StandardServerTip -> LedgerState -> App ()
reportRollback serverTip ledgerState = do
  let
    -- Get the block number
    rollbackBlockNo =
      maybe "Genesis" (textShow . Block.unBlockNo)
        . Block.withOriginToMaybe
        . headerStateBlockNo
        . headerState
        . Cfg.unLedgerState
        $ ledgerState

    -- Tip block number
    serverBlockNo =
      case Cfg.unServerTip serverTip of
        TipGenesis -> "Genesis"
        Tip _ _ (Block.BlockNo serverBlockNo') -> textShow serverBlockNo'

  putStr $
    "\rRolled back block: "
      <> rollbackBlockNo
      <> " of "
      <> serverBlockNo
      <> "          " -- Add a some extra space to avoid garbled output
  hFlush stdout

finish :: App ()
finish = do
  tracer <- asks Cfg.cfgTrace

  liftIO $ do
    putTextLn "" -- Clear the status line
    logError (appendName "Reactor" tracer) "Received shutdown signal"

  writeLedgerSnapshot
