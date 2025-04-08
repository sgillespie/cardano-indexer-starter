module Cardano.Indexer.Config
  ( AppT (..),
    App,
    Config (..),
    NetworkMagic (..),
    TestnetMagic (..),
    SocketPath (..),
    NodeConfigFile (..),
    StandardBlock,
    StandardTip,
    AppError(..),
    runAppT,
    networkMagicId,
  ) where

import Cardano.Ledger.Crypto (StandardCrypto)
import Text.Show (Show(..))
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Node (ProtocolInfo)
import Ouroboros.Network.Block (Tip)

newtype AppT m a = AppT {runApp :: ReaderT Config m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Config
    )

type App = AppT IO

data Config = Config
  { cfgMagic :: NetworkMagic,
    cfgSocketPath :: SocketPath,
    cfgProtocolInfo :: ProtocolInfo StandardBlock
  }

data NetworkMagic
  = Mainnet
  | Testnet TestnetMagic
  deriving stock (Eq, Show)

newtype TestnetMagic = TestnetMagic {unNetworkMagic :: Word32}
  deriving stock (Eq, Show)

newtype SocketPath = SocketPath {unSocketPath :: FilePath}
  deriving stock (Eq, Show)

newtype NodeConfigFile = NodeConfigFile {unNodeConfigFile :: FilePath}
  deriving stock (Eq, Show)

type StandardBlock = CardanoBlock StandardCrypto

type StandardTip = Tip StandardBlock

data AppError
  = NodeConfigError Text
  | ImpossibleError
  deriving stock (Typeable)

instance Exception AppError

instance Show AppError where
  show (NodeConfigError err) = "Node configuration error: " <> toString err
  show ImpossibleError = "The impossible occurred!"

runAppT :: AppT m a -> Config -> m a
runAppT = runReaderT . runApp

networkMagicId :: NetworkMagic -> Word32
networkMagicId Mainnet = 764824073
networkMagicId (Testnet (TestnetMagic magic)) = magic
