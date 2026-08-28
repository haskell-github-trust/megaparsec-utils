{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Text.Megaparsec.Utils.Char
-- Description : Parsers for streams of 'Char'.
-- Copyright   : (c) drlkf, 2024
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Generic parsers for streams of 'Char' ('String', 'Text', lazy 'Text').
module Text.Megaparsec.Utils.Char (
  -- * Scalar parsers
  boolParser,
  numParser,
  posDecNumParser,
  posNumParser,
  uuidParser,

  -- * Combinators
  commaSeparated,

  -- * Compatibility utilities
  boundedEnumShowParser,
  parsecToJSONParser,
) where

import Data.Aeson.Types (Parser, Value, withText)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.UUID (UUID)
import Text.Megaparsec (
  MonadParsec,
  Parsec,
  ShowErrorComponent,
  Token,
  errorBundlePretty,
  runParser,
 )
import Text.Megaparsec.Char (char, char', digitChar, hexDigitChar)
import Text.Megaparsec.Utils.Common (
  mkBoolParser,
  mkBoundedEnumShowParser,
  mkCommaSeparated,
  mkNumParser,
  mkPosDecNumParser,
  mkPosNumParser,
  mkUuidParser,
 )

-- | Parse a case-insensitive human-readable boolean, including C-style numbers,
-- English yes-no and @on@ / @off@.
boolParser :: (MonadParsec e s m, Token s ~ Char) => m Bool
boolParser = mkBoolParser char' id

-- | Parse a 'Bounded' 'Enum' type that has a 'Show' instance, trying all
-- possibilities, case-insensitive, in the 'Enum' order.
boundedEnumShowParser
  :: forall a e s m
   . (MonadParsec e s m, Token s ~ Char)
  => Bounded a
  => Enum a
  => Show a
  => m a
boundedEnumShowParser = mkBoundedEnumShowParser char' id

-- | Parse a comma-separated list of items.
commaSeparated
  :: (MonadParsec e s m, Token s ~ Char)
  => m a
  -> m (NonEmpty a)
commaSeparated = mkCommaSeparated (char ',')

-- | Parse a positive number, with or without decimals prefixed by a @.@.
posDecNumParser
  :: (MonadFail m, MonadParsec e s m, Read a, Token s ~ Char)
  => m a
posDecNumParser = mkPosDecNumParser digitChar (char '.') id

-- | Parse a positive integer.
posNumParser
  :: (MonadFail m, MonadParsec e s m, Read a, Token s ~ Char)
  => m a
posNumParser = mkPosNumParser digitChar id

-- | Parse an integer, without any space between minus sign and digits.
numParser
  :: (MonadFail m, MonadParsec e s m, Num a, Read a, Token s ~ Char)
  => m a
numParser = mkNumParser (char '-') posNumParser

-- | Convert a 'Parsec' parser on 'Text' into a 'Parser' suited for
-- 'Data.Aeson.FromJSON' instances.
parsecToJSONParser
  :: ShowErrorComponent e
  => String
  -- ^ Parser name.
  -> Parsec e Text a
  -- ^ Parser.
  -> (Value -> Parser a)
parsecToJSONParser n p =
  withText n (either (fail . errorBundlePretty) pure . runParser p n)

-- | Parse a RFC4122-compliant UUID.
uuidParser
  :: (MonadParsec e s m, Token s ~ Char)
  => m UUID
uuidParser = mkUuidParser hexDigitChar (char '-') id