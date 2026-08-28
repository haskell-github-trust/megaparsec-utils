{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Text.Megaparsec.Utils
-- Description : Various generic parsers and combinators.
-- Copyright   : (c) drlkf, 2024
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- String-based shims over 'Text.Megaparsec.Utils.Char'. New code should use
-- 'Text.Megaparsec.Utils.Char' or 'Text.Megaparsec.Utils.Byte' directly.
module Text.Megaparsec.Utils (
  -- * Scalar parsers
  boolParser,
  numParser,
  posDecNumParser,
  posNumParser,
  uuidParser,

  -- * Combinators
  commaSeparated,
  occurrence,
  occurrences,

  -- * Compatibility utilities
  boundedEnumShowParser,
  parsecToReadsPrec,
  parsecToJSONParser,
) where

import Data.Aeson.Types (Parser, Value, withText)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T (unpack)
import Data.UUID (UUID)
import Text.Megaparsec (
  Parsec,
  ShowErrorComponent,
  errorBundlePretty,
  runParser,
 )
import qualified Text.Megaparsec.Utils.Char as Char
import qualified Text.Megaparsec.Utils.Common as Common

-- | Parse a case-insensitive human-readable boolean, including C-style numbers,
-- English yes-no and @on@ / @off@.
boolParser
  :: Ord e
  => Parsec e String Bool
boolParser = Char.boolParser

-- | Parse a 'Bounded' 'Enum' type that has a 'Show' instance, trying all
-- possibilities, case-insensitive, in the 'Enum' order.
boundedEnumShowParser
  :: forall a e
   . Ord e
  => Bounded a
  => Enum a
  => Show a
  => Parsec e String a
boundedEnumShowParser = Char.boundedEnumShowParser

-- | Parse a comma-separated list of items.
commaSeparated
  :: Ord e
  => Parsec e String a
  -> Parsec e String (NonEmpty a)
commaSeparated = Char.commaSeparated

-- | Parse any occurrence of a given parser. Consumes any input before occurrence.
occurrence
  :: Ord e
  => Parsec e String a
  -> Parsec e String a
occurrence = Common.occurrence

-- | Parse all occurrences of a given parser.
occurrences
  :: Ord e
  => Parsec e String a
  -> Parsec e String [a]
occurrences = Common.occurrences

-- | Parse a positive number, with or without decimals prefixed by a @.@.
posDecNumParser
  :: Ord e
  => Read a
  => Parsec e String a
posDecNumParser = Char.posDecNumParser

-- | Parse a positive integer.
posNumParser
  :: Ord e
  => Read a
  => Parsec e String a
posNumParser = Char.posNumParser

-- | Parse an integer, without any space between minus sign and digits.
numParser
  :: Ord e
  => Num a
  => Read a
  => Parsec e String a
numParser = Char.numParser

-- | Convert a 'Parsec' parser into a 'Parser' suited for 'Data.Aeson.FromJSON'
-- instances.
parsecToJSONParser
  :: ShowErrorComponent e
  => String
  -- ^ Parser name.
  -> Parsec e String a
  -- ^ Parser.
  -> (Value -> Parser a)
parsecToJSONParser n p =
  withText n (either (fail . errorBundlePretty) pure . runParser p n . T.unpack)

-- | Convert a 'Parsec' parser into a 'ReadS' parser. Useful for defining 'Read'
-- instances with 'Text.Megaparsec'.
parsecToReadsPrec
  :: Parsec e String a
  -> ReadS a
parsecToReadsPrec p = either (const []) (\x -> [(x, "")]) . runParser p "string"

-- | Parse a RFC4122-compliant UUID.
uuidParser
  :: Ord e
  => Parsec e String UUID
uuidParser = Char.uuidParser
