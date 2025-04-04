module Cardano.Indexer.Config
  ( AppT (..),
    App,
    Config (..),
    NetworkMagic (..),
    TestnetMagic (..),
    SocketPath (..),
    StandardBlock,
    StandardTip,
    runAppT,
    networkMagicId,
  ) where
import Ouroboros.Consensus.Node (ProtocolInfo)
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Cardano.Ledger.Crypto (StandardCrypto)
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

newtype SocketPath = SocketPath { unSocketPath :: FilePath }
  deriving stock (Eq, Show)

type StandardBlock = CardanoBlock StandardCrypto

type StandardTip = Tip StandardBlock

runAppT :: AppT m a -> Config -> m a

runAppT = runReaderT . runApp

networkMagicId :: NetworkMagic -> Word32
networkMagicId Mainnet = 764824073
networkMagicId (Testnet (TestnetMagic magic)) = magic
