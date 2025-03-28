module Cardano.Indexer
  ( AppT (..),
    App,
    Config (..),
    runAppT,
    runIndexer,
  ) where

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

runAppT :: AppT m a -> Config -> m a
runAppT = runReaderT . runApp

runIndexer :: Config -> IO ()
runIndexer = runAppT indexer

indexer :: App ()
indexer = pure ()
