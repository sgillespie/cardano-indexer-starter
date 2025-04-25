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

import Control.Concurrent.Class.MonadSTM.Strict (readTBQueue, readTVar, writeTVar)
import Ouroboros.Consensus.Block (BlockNo (..), WithOrigin (..), blockNo)
import Ouroboros.Consensus.Ledger.Abstract (tickThenReapply)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..))
import Ouroboros.Consensus.Node (ProtocolInfo (..))
import Ouroboros.Network.Block (Point (..), Tip (..), getTipBlockNo)
import Ouroboros.Network.Point (WithOrigin (..), withOrigin)
import UnliftIO (atomically, hFlush)
import Prelude hiding (atomically)

runReactor :: App ()
runReactor = loop =<< asks Cfg.cfgEvents
  where
    loop queue = do
      msg <- atomically $ readTBQueue (Cfg.unReactorQueue queue)
      res <- runReactorAction msg
      case msg of
        Cfg.Finish -> pure res
        _ -> loop queue

runReactorAction :: ReactorActions -> App ()
runReactorAction (Cfg.WriteBlock serverTip block) = writeBlock serverTip block
runReactorAction Cfg.Finish = pure ()
runReactorAction (Cfg.RollbackBlock serverTip point whenFinished) = rollbackBlock serverTip point whenFinished

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
    ledgerState <- readTVar ledger

    let
      ledgerState' = applyLedgerState protoInfo block ledgerState
    writeTVar ledger ledgerState'
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

reportBlock :: MonadIO io => StandardServerTip -> StandardBlock -> io ()
reportBlock (Cfg.ServerTip serverTip) clientBlock = do
  let
    -- Get the block number
    (BlockNo blockNo') = blockNo clientBlock
    -- Tip block number
    serverBlockNo =
      case serverTip of
        TipGenesis -> "Genesis"
        Tip _ _ (BlockNo serverBlockNo') -> textShow serverBlockNo'
    -- Are we close the tip?
    upToDate = blockNo' + 500 >= withOrigin 0 unBlockNo (getTipBlockNo serverTip)

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

  let
    -- Get the block number
    rollbackBlockNo =
      case getPoint point of
        Origin -> "Genesis"
        At block -> textShow block

    -- Tip block number
    serverBlockNo =
      case Cfg.unServerTip serverTip of
        TipGenesis -> "Genesis"
        Tip _ _ (BlockNo serverBlockNo') -> textShow serverBlockNo'

  -- Roll back ledger state
  ledgerState <- asks Cfg.cfgLedgerState
  let
    newState = Cfg.LedgerState (pInfoInitLedger protoInfo)
  atomically $ writeTVar ledgerState newState

  putStr $
    "\rRolled back to block: "
      <> rollbackBlockNo
      <> " of "
      <> serverBlockNo
      <> "          " -- Add a some extra space to avoid garbled output
  hFlush stdout

  liftIO $ whenFinished serverTip point
