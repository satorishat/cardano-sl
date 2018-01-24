-- | Functions related to Block/Software versions.

module Pos.Core.Update.Version
       ( parseBlockVersion
       , parseSoftwareVersion
       ) where

import           Universum

import           Serokell.Util.Parse (parseIntegralSafe)
import           Text.Parsec (try)
import           Text.Parsec.Char (alphaNum, char, letter, string)
import           Text.Parsec.Combinator (manyTill)
import           Text.Parsec.Text (Parser)

import           Pos.Core.Update.Types (BlockVersion (..), SoftwareVersion (..),
                                        ApplicationName (..))
import           Pos.Util.Orphans ()

parseBlockVersion :: Parser BlockVersion
parseBlockVersion = do
    bvMajor <- parseIntegralSafe
    _       <- char '.'
    bvMinor <- parseIntegralSafe
    _       <- char '.'
    bvAlt   <- parseIntegralSafe
    return BlockVersion{..}

parseSoftwareVersion :: Parser SoftwareVersion
parseSoftwareVersion = do
    svAppName <-
        pure . ApplicationName . toText =<<
        ((:) <$> letter <*> manyTill (alphaNum <|> char '-') (try $ string ":"))
    svNumber <- parseIntegralSafe
    return SoftwareVersion {..}
