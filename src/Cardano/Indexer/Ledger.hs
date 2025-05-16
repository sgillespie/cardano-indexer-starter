module Cardano.Indexer.Ledger
  ( writeLedgerSnapshot,
  ) where

import Cardano.Indexer.Config (App, LedgerState, StandardBlock)
import Cardano.Indexer.Config qualified as Cfg

import Cardano.BM.Trace (appendName, logError)
import Cardano.Binary (serialize')
import Cardano.Ledger.BaseTypes (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (readTVar)
import Data.ByteString (writeFile)
import Ouroboros.Consensus.Block (Point (..))
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.HeaderValidation (headerStatePoint)
import Ouroboros.Consensus.Ledger.Extended
  ( ExtLedgerState (headerState),
    encodeDiskExtLedgerState,
  )
import Ouroboros.Consensus.Node (ProtocolInfo (..))
import System.FilePath ((<.>), (</>))
import UnliftIO qualified
import UnliftIO.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)

writeLedgerSnapshot :: App ()
writeLedgerSnapshot = do
  tracer <- asks (appendName "Ledger" . Cfg.cfgTrace)

  -- Read ledger state
  ledgerState <- do
    ledger' <- asks Cfg.cfgLedgerState
    UnliftIO.atomically (readTVar ledger')
  protoInfo <- asks Cfg.cfgProtocolInfo

  -- Generate ledger file path
  ledgerDir' <- ledgerDir
  createDirectoryIfMissing True ledgerDir'
  let
    ledgerFile = ledgerSnapshotFile ledgerState ledgerDir'

  -- Serialize the ledger state
  let
    ledgerBytes = serializeLedger protoInfo ledgerState

  liftIO $ do
    writeFile ledgerFile ledgerBytes
    logError tracer $ "Saved ledger state file: " <> toText ledgerFile

serializeLedger :: ProtocolInfo StandardBlock -> LedgerState -> ByteString
serializeLedger protoInfo (Cfg.LedgerState ledgerState) =
  serialize' $ encodeDiskExtLedgerState codecConfig ledgerState
  where
    codecConfig = topLevelConfigCodec . pInfoConfig $ protoInfo

ledgerDir :: App FilePath
ledgerDir = do
  stateDir <- getXdgDirectory XdgState "cardano-indexer-starter"
  pure (stateDir </> "ledger")

ledgerSnapshotFile :: LedgerState -> FilePath -> FilePath
ledgerSnapshotFile ledgerState baseDir =
  baseDir </> "snapshot-" <> slotNo <.> "state"
  where
    header = headerState (Cfg.unLedgerState ledgerState)
    slotNo =
      case headerStatePoint header of
        GenesisPoint -> "0"
        BlockPoint (SlotNo slotNo') _ -> show slotNo'
