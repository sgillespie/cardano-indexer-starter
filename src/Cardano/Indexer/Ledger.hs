module Cardano.Indexer.Ledger
  ( writeLedgerSnapshot,
    readLedgerSnapshot,
  ) where

import Cardano.Indexer.Config (App, LedgerState, StandardBlock)
import Cardano.Indexer.Config qualified as Cfg

import Cardano.BM.Trace (appendName, logError)
import Cardano.Binary (DecoderError, decodeFullDecoder', serialize')
import Cardano.Ledger.BaseTypes (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (readTVar)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe)
import Data.ByteString (readFile, writeFile)
import Data.List.Extra (maximumOn)
import Ouroboros.Consensus.Block (Point (..))
import Ouroboros.Consensus.Config (TopLevelConfig (..))
import Ouroboros.Consensus.HeaderValidation (headerStatePoint)
import Ouroboros.Consensus.Ledger.Extended
  ( ExtLedgerState (headerState),
    decodeDiskExtLedgerState,
    encodeDiskExtLedgerState,
  )
import Ouroboros.Consensus.Node (ProtocolInfo (..))
import System.Directory.Extra (listFiles)
import System.FilePath (dropExtension, takeExtension, takeFileName, (<.>), (</>))
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

readLedgerSnapshot :: App (Maybe LedgerState)
readLedgerSnapshot = do
  protoInfo <- asks Cfg.cfgProtocolInfo
  tracer <- asks (appendName "Ledger" . Cfg.cfgTrace)

  -- Look up ledger state dir
  ledgerDir' <- ledgerDir
  allFiles <- liftIO (listFiles ledgerDir')

  -- Filter state files
  let
    stateFiles = filter (\f -> takeExtension f == ".state") allFiles
    newestStateFile = maximumOnMay findSlotNo stateFiles

  ledgerState <- runMaybeT $ do
    stateFile <- hoistMaybe newestStateFile
    -- Load snapshot into memory
    bytes <- liftIO (readFile stateFile)

    -- Deserialize ledger state
    case deserializeLedger protoInfo bytes of
      Right l -> do
        liftIO . logError tracer $ "Loaded state file: " <> toText stateFile
        hoistMaybe (Just l)
      Left err -> do
        liftIO . logError tracer $ "Could not read state file: " <> show err <> "!"
        hoistMaybe Nothing

  when (isNothing ledgerState) $
    liftIO (logError tracer "Starting from genesis")

  pure ledgerState
  where
    maximumOnMay _ [] = Nothing
    maximumOnMay f xs = Just (maximumOn f xs)

    findSlotNo =
      (readMaybe @Int =<<)
        . tailMay -- Drop leading '-'
        . dropWhile (/= '-')
        . dropExtension
        . takeFileName

deserializeLedger
  :: ProtocolInfo StandardBlock
  -> ByteString
  -> Either DecoderError LedgerState
deserializeLedger protoInfo =
  fmap Cfg.LedgerState
    . decodeFullDecoder' "LedgerState" (decodeDiskExtLedgerState codecConfig)
  where
    codecConfig = topLevelConfigCodec . pInfoConfig $ protoInfo

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
