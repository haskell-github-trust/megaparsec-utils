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
module Text.Megaparsec.Utils (
  boolParser,
  boundedEnumShowParser,
  commaSeparated,
  numParser,
  occurrence,
  occurrences,
  parsecToJSONParser,
  parsecToReadsPrec,
  posDecNumParser,
  posNumParser,
  uuidParser,
) where

import Control.Applicative (many, some, (<|>))
import Control.Applicative.Combinators (choice)
import Control.Monad (replicateM)
import Control.Monad.Combinators (optional)
import Data.Aeson.Types (Parser, Value, withText)
import Data.Functor (($>))
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import Data.UUID (UUID)
import qualified Data.UUID as U (fromString)
import Text.Megaparsec (
  Parsec,
  ShowErrorComponent,
  anySingle,
  errorBundlePretty,
  runParser,
  try,
 )
import Text.Megaparsec.Char (
  char,
  digitChar,
  hexDigitChar,
  string',
 )

-- | Parse a case-insensitive human-readable boolean, including C-style numbers
-- and English yes-no.
boolParser
  :: Ord e
  => Parsec e String Bool
boolParser = true <|> false
 where
  true = True <$ choice (map string' ["true", "y", "yes", "1"])
  false = False <$ choice (map string' ["false", "n", "no", "0"])

-- | Parse a 'Bounded' 'Enum' type that has a 'Show' instance, trying all
-- possibilities, case-insensitive, in the 'Enum' order.
boundedEnumShowParser
  :: forall a e
   . Ord e
  => Bounded a
  => Enum a
  => Show a
  => Parsec e String a
boundedEnumShowParser =
  choice . map parseShow $ sortOn (negate . length . show) [(minBound :: a) ..]
 where
  parseShow a = string' (show a) $> a

-- | Parse a comma-separated list of items.
commaSeparated
  :: Ord e
  => Parsec e String a
  -> Parsec e String (NonEmpty a)
commaSeparated p = (:|) <$> p <*> many (char ',' >> p)

-- | Parse any occurrence of a given parser. Consumes any input before occurrence.
occurrence
  :: Ord e
  => Parsec e String a
  -> Parsec e String a
occurrence p = go
 where
  go = p <|> (anySingle >> go)

-- | Parse all occurrences of a given parser.
occurrences
  :: Ord e
  => Parsec e String a
  -> Parsec e String [a]
occurrences = some . try . occurrence . try

-- | Parse a positive number with decimals.
posDecNumParser
  :: Ord e
  => Parsec e String Double
posDecNumParser = do
  num <- some digitChar
  den <- maybe "" ("." <>) <$> optional (char '.' >> some digitChar)

  return . read $ num <> den

-- | Parse a positive integer.
posNumParser
  :: Ord e
  => Read a
  => Parsec e String a
posNumParser = read <$> some digitChar

-- | Parse an integer, without any space between minus sign and digits.
numParser
  :: Ord e
  => Num a
  => Read a
  => Parsec e String a
numParser = (char '-' >> negate <$> posNumParser) <|> posNumParser

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
  withText n $ either (fail . errorBundlePretty) pure . runParser p n . T.unpack

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
uuidParser = do
  part1 <- replicateM 8 hexDigitChar <* char '-'
  part2 <- replicateM 4 hexDigitChar <* char '-'
  part3 <- replicateM 4 hexDigitChar <* char '-'
  part4 <- replicateM 4 hexDigitChar <* char '-'
  part5 <- replicateM 12 hexDigitChar

  pure . fromJust . U.fromString $ intercalate "-" [part1, part2, part3, part4, part5]
