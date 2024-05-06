{-# LANGUAGE TypeFamilies #-}

module Text.Megaparsec.Utils
  ( boolParser
  , boundedEnumShowParser
  , commaSeparated
  , numParser
  , occurrence
  , occurrences
  , parsecToJSONParser
  , parsecToReadsPrec
  , posDecNumParser
  , posNumParser
  , uuidParser
  ) where

import           Control.Applicative             (many, some, (<|>))
import           Control.Applicative.Combinators (choice)
import           Control.Monad                   (replicateM, void)
import           Control.Monad.Combinators       (optional)
import           Data.Aeson.Types                (Parser, Value, withText)
import           Data.Functor                    (($>))
import           Data.List                       (intercalate, sortOn)
import           Data.List.NonEmpty              (NonEmpty ((:|)))
import           Data.Maybe                      (fromJust)
import qualified Data.Text                       as T (unpack)
import           Data.UUID                       (UUID)
import qualified Data.UUID                       as U (fromString)
import           Data.Void                       (Void)
import           Text.Megaparsec                 (Parsec, anySingle,
                                                  errorBundlePretty, runParser,
                                                  try)
import           Text.Megaparsec.Char            (char, digitChar, hexDigitChar,
                                                  string')

-- | Parse a case-insensitive human-readable boolean, including C-style numbers
-- and English yes-no.
boolParser :: Parsec Void String Bool
boolParser = true <|> false
  where true  = True  <$ choice (map string' ["true", "y", "yes", "1"])
        false = False <$ choice (map string' ["false", "n", "no", "0"])

-- | Parse a 'Bounded' 'Enum' type that has a 'Show' instance, trying all
-- possibilities, case-insensitive, in the 'Enum' order.
boundedEnumShowParser
  :: Bounded a
  => Enum a
  => Show a
  => Parsec Void String a
boundedEnumShowParser =
  choice . map parseShow $ sortOn (negate . length . show) [minBound ..]
  where parseShow a = string' (show a) $> a

-- | Parse a comma-separated list of items.
commaSeparated :: Parsec Void String a -> Parsec Void String (NonEmpty a)
commaSeparated p = (:|) <$> p <*> many (char ',' >> p)

-- | Parse any occurrence of a given parser. Consumes any input before occurence.
occurrence :: Parsec Void String a -> Parsec Void String a
occurrence p = go
  where go = p <|> (anySingle >> go)

-- | Parse all occurrences of a given parser.
occurrences :: Parsec Void String a -> Parsec Void String [a]
occurrences = some . try . occurrence

-- | Parse a positive number with decimals.
posDecNumParser :: Parsec Void String Double
posDecNumParser = do
  num <- some digitChar
  den <- maybe "" ("." <>) <$> optional (char '.' >> some digitChar)

  return . read $ num <> den

-- | Parse a positive integer.
posNumParser :: Read a => Parsec Void String a
posNumParser = read <$> some digitChar

-- | Parse an integer, without any spaces between minus sign and digits.
numParser :: Parsec Void String Int
numParser = (char '-' >> negate <$> posNumParser) <|> posNumParser

-- | Convert a 'Parsec' parser into a 'Parser' suited for 'FromJSON' instances.
parsecToJSONParser
  :: String
  -> Parsec Void String a
  -> Value
  -> Parser a
parsecToJSONParser n p =
  withText n $ either (fail . errorBundlePretty) pure . runParser p n . T.unpack

-- | Convert a 'Parsec' parser into a 'ReadS' parser. Useful for defining 'Read'
-- instances with 'Megaparsec'.
parsecToReadsPrec :: Parsec Void String a -> ReadS a
parsecToReadsPrec p = either (const []) (\x -> [(x, "")]) . runParser p "string"

-- | Parse a RFC4122-compliant UUID.
uuidParser :: Parsec Void String UUID
uuidParser = do
  part1 <- replicateM 8 hexDigitChar
  void $ char '-'
  part2 <- replicateM 4 hexDigitChar
  void $ char '-'
  part3 <- replicateM 4 hexDigitChar
  void $ char '-'
  part4 <- replicateM 4 hexDigitChar
  void $ char '-'
  part5 <- replicateM 12 hexDigitChar

  pure . fromJust . U.fromString $ intercalate "-" [part1, part2, part3, part4, part5]
