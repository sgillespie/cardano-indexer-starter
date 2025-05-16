module Cardano.Indexer.Config
  ( AppT (..),
    App,
    Config (..),
    NetworkMagic (..),
    TestnetMagic (..),
    SocketPath (..),
    ReactorQueue (..),
    ReactorActions (..),
    ServerTip (..),
    LedgerState (..),
    NodeConfigFile (..),
    TopologyConfigFile (..),
    DatabaseDir (..),
    StandardBlock,
    StandardTip,
    StandardPoint,
    StandardServerTip,
    AppError (..),
    runAppT,
    networkMagicId,
  ) where

import Cardano.BM.Trace (Trace)
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Concurrent.Class.MonadSTM.Strict (StrictTBQueue, StrictTVar)
import Ouroboros.Consensus.Block (Point)
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import Ouroboros.Consensus.Node (ProtocolInfo)
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Tip)
import Text.Show (Show (..))
import UnliftIO (MonadUnliftIO)

newtype AppT m a = AppT {runApp :: ReaderT (Config m) m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Config m),
      MonadUnliftIO
    )

type App = AppT IO

data Config m = Config
  { cfgMagic :: NetworkMagic,
    cfgSocketPath :: SocketPath,
    cfgProtocolInfo :: ProtocolInfo StandardBlock,
    cfgTrace :: Trace m Text,
    cfgEvents :: ReactorQueue m,
    cfgLedgerState :: StrictTVar m LedgerState
  }

data NetworkMagic
  = Mainnet
  | Testnet TestnetMagic
  deriving stock (Eq, Show)

newtype TestnetMagic = TestnetMagic {unNetworkMagic :: Word32}
  deriving stock (Eq, Show)

newtype SocketPath = SocketPath {unSocketPath :: FilePath}
  deriving stock (Eq, Show)

newtype ReactorQueue m = ReactorQueue {unReactorQueue :: StrictTBQueue m ReactorActions}

data ReactorActions
  = Init (StandardPoint -> IO ())
  | WriteBlock StandardServerTip StandardBlock
  | RollbackBlock
      StandardServerTip
      StandardPoint
      ( StandardServerTip
        -> StandardPoint
        -> IO ()
      )
  | Finish

newtype ServerTip blk = ServerTip {unServerTip :: Tip blk}
  deriving stock (Eq, Show)

type StandardServerTip = ServerTip StandardBlock

newtype LedgerState = LedgerState {unLedgerState :: ExtLedgerState StandardBlock}
  deriving stock (Eq, Show)

newtype NodeConfigFile = NodeConfigFile {unNodeConfigFile :: FilePath}
  deriving stock (Eq, Show)

newtype TopologyConfigFile = TopologyConfigFile {unTopologyConfigFile :: FilePath}
  deriving stock (Eq, Show)

newtype DatabaseDir = DatabaseDir {unDatabaseDir :: FilePath}
  deriving stock (Eq, Show)

type StandardBlock = CardanoBlock StandardCrypto

type StandardTip = Tip StandardBlock

type StandardPoint = Point StandardBlock

data AppError
  = NodeConfigError Text
  | ImpossibleError
  deriving stock (Typeable)

instance Exception AppError

instance Show AppError where
  show (NodeConfigError err) = "Node configuration error: " <> toString err
  show ImpossibleError = "The impossible occurred!"

runAppT :: AppT m a -> Config m -> m a
runAppT = runReaderT . runApp

networkMagicId :: NetworkMagic -> Word32
networkMagicId Mainnet = 764824073
networkMagicId (Testnet (TestnetMagic magic)) = magic
