module Main where

import Cardano.Indexer (Config (..), runIndexer)

main :: IO ()
main = runIndexer Config
