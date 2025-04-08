module Cardano.Indexer.CLI
  ( Options (..),
    NetworkMagic (..),
    TestnetMagic (..),
    parseOptions,
  ) where

import Cardano.Indexer.Config
  ( NetworkMagic (..),
    NodeConfigFile (..),
    SocketPath (..),
    TestnetMagic (..),
  )

import Options.Applicative (Parser)
import Options.Applicative qualified as Opts

data Options = Options
  { optNetworkMagic :: NetworkMagic,
    optNodeConfig :: NodeConfigFile,
    optSocketPath :: SocketPath
  }

parseOptions :: IO Options
parseOptions = Opts.execParser (Opts.info parser desc)
  where
    parser = options <**> Opts.helper
    desc = Opts.fullDesc <> Opts.progDesc "A sample Cardano indexer"

options :: Parser Options
options =
  Options
    <$> parseNetworkMagic
    <*> parseNodeConfigFile
    <*> parseSocketPath

parseNetworkMagic :: Parser NetworkMagic
parseNetworkMagic = parseMainnet <|> parseTestnetMagic

parseMainnet :: Parser NetworkMagic
parseMainnet = Opts.flag' Mainnet optionMod
  where
    optionMod =
      Opts.long "mainnet"
        <> Opts.short 'm'
        <> Opts.help "Use the mainnet magic id."

parseTestnetMagic :: Parser NetworkMagic
parseTestnetMagic = Testnet . TestnetMagic <$> Opts.option Opts.auto optionMod
  where
    optionMod =
      Opts.long "testnet-magic"
        <> Opts.short 't'
        <> Opts.metavar "NATURAL"
        <> Opts.help "Specify the testnet magic id."

parseNodeConfigFile :: Parser NodeConfigFile
parseNodeConfigFile = NodeConfigFile <$> Opts.strOption optionMod
  where
    optionMod =
      Opts.long "node-config"
        <> Opts.short 'f'
        <> Opts.metavar "PATH"
        <> Opts.help "Configuration file for cardano-node."

parseSocketPath :: Parser SocketPath
parseSocketPath = SocketPath <$> Opts.strOption optionMod
  where
    optionMod =
      Opts.long "socker-path"
        <> Opts.short 's'
        <> Opts.metavar "PATH"
        <> Opts.help "Path to the node socket."
