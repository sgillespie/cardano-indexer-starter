module Main where

import Cardano.Indexer (parseOptions, runIndexer)

main :: IO ()
main = runIndexer =<< parseOptions
