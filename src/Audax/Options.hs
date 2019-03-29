-- | Helpers for optparse-applicative commands.
{-# LANGUAGE NoImplicitPrelude #-}
module Audax.Options where

import           Data.Bifunctor      (first)
import qualified Data.Yaml           as Yaml
import           Options.Applicative

import Audax.Prelude


-- $setup
-- >>> import Text.Show.Pretty (pPrint)
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> :set -interactive-print pPrint


-- | Like @Options.Applicative.helper@ except it only supports @--help@, freeing
-- up @-h@ for other uses (for instance, as Postgres @HOSTNAME@)
longHelper :: Parser (a -> a)
longHelper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , help "Show this help text"
  , hidden
  ]


descHeader :: String -> InfoMod a
descHeader txt = progDesc txt `mappend` header txt


-- | Parse a yaml config file location from cli args.
parseCfgFile :: Parser FilePath
parseCfgFile =
  strArgument $ mconcat
    [ help "YAML config file"
    , metavar "FILE"
    ]


-- | Parse a yaml config file into a SrcOfTruthCfg or throw an error.
getYamlFile :: Yaml.FromJSON a => FilePath -> IO a
getYamlFile file =
  Yaml.decodeFileEither file >>=
    either
      (fail . Yaml.prettyPrintParseException)
      return


-- | Parse a yaml config bytes.
decodeYaml :: Yaml.FromJSON a => ByteString -> Either String a
decodeYaml = first Yaml.prettyPrintParseException . Yaml.decodeEither'
