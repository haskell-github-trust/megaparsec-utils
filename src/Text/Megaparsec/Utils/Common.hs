{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Text.Megaparsec.Utils.Common
-- Description : Stream-agnostic combinators.
-- Copyright   : (c) drlkf, 2024
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Combinators that work on any stream.
module Text.Megaparsec.Utils.Common (
  occurrence,
  occurrences,

  -- * Shared parser builders
  mkBoolParser,
  mkBoundedEnumShowParser,
  mkCommaSeparated,
  mkNumParser,
  mkPosDecNumParser,
  mkPosNumParser,
  mkUuidParser,
) where

import Control.Applicative (many, some, (<|>))
import Control.Applicative.Combinators (choice)
import Control.Monad (replicateM)
import Control.Monad.Combinators (optional)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust)
import Data.UUID (UUID)
import qualified Data.UUID as U (fromString)
import Text.Megaparsec (MonadParsec, Token, anySingle, try)
import Text.Read (readMaybe)

-- | Parse any occurrence of a given parser. Consumes any input before occurrence.
occurrence :: MonadParsec e s m => m a -> m a
occurrence p = go
 where
  go = p <|> (anySingle >> go)

-- | Parse all occurrences of a given parser.
occurrences :: MonadParsec e s m => m a -> m [a]
occurrences = some . try . occurrence . try

-- | Build a case-insensitive human-readable boolean parser, including C-style
-- numbers, English yes-no and @on@ / @off@.
mkBoolParser
  :: MonadParsec e s m
  => (Token s -> m (Token s))
  -- ^ Case-insensitive single-token parser.
  -> (Char -> Token s)
  -- ^ Convert a literal 'Char' to a token.
  -> m Bool
mkBoolParser char' fromChar = true <|> false
 where
  true = True <$ choice (map (try . traverse_ (char' . fromChar)) ["true", "y", "yes", "on", "1"])
  false = False <$ choice (map (try . traverse_ (char' . fromChar)) ["false", "n", "no", "off", "0"])

-- | Build a parser for a 'Bounded' 'Enum' type that has a 'Show' instance,
-- trying all possibilities, case-insensitive, in the 'Enum' order.
mkBoundedEnumShowParser
  :: forall a e s m
   . (MonadParsec e s m)
  => Bounded a
  => Enum a
  => Show a
  => (Token s -> m (Token s))
  -- ^ Case-insensitive single-token parser.
  -> (Char -> Token s)
  -- ^ Convert a literal 'Char' to a token.
  -> m a
mkBoundedEnumShowParser char' fromChar =
  choice . map parseShow $ sortOn (negate . length . show) [(minBound :: a) ..]
 where
  parseShow a = try (traverse_ (char' . fromChar) (show a)) $> a

-- | Build a parser for a comma-separated list of items.
mkCommaSeparated
  :: MonadParsec e s m
  => m (Token s)
  -- ^ Comma parser.
  -> m a
  -> m (NonEmpty a)
mkCommaSeparated comma p = (:|) <$> p <*> many (comma >> p)

-- | Build a parser for a positive number, with or without decimals prefixed by
-- a @.@.
mkPosDecNumParser
  :: (MonadFail m, MonadParsec e s m, Read a)
  => m (Token s)
  -- ^ Digit parser.
  -> m (Token s)
  -- ^ Decimal-point parser.
  -> (Token s -> Char)
  -- ^ Convert a token to a 'Char'.
  -> m a
mkPosDecNumParser digitChar dot toChar = do
  num <- some digitChar
  dec <- optional (dot >> some digitChar)

  let toStr = map toChar
      str = toStr num <> maybe "" (('.' :) . toStr) dec

  maybe (fail ("could not read from input: " <> str)) pure (readMaybe str)

-- | Build a parser for a positive integer.
mkPosNumParser
  :: (MonadFail m, MonadParsec e s m, Read a)
  => m (Token s)
  -- ^ Digit parser.
  -> (Token s -> Char)
  -- ^ Convert a token to a 'Char'.
  -> m a
mkPosNumParser digitChar toChar = do
  digits <- some digitChar
  let str = map toChar digits
  maybe
    (fail ("could not read from digits: " <> str))
    pure
    (readMaybe str)

-- | Build a parser for an integer, without any space between minus sign and
-- digits.
mkNumParser
  :: (MonadFail m, MonadParsec e s m, Num a, Read a)
  => m (Token s)
  -- ^ Minus-sign parser.
  -> m a
  -- ^ Positive-number parser.
  -> m a
mkNumParser minus posNumParser = (minus >> negate <$> posNumParser) <|> posNumParser

-- | Build a parser for a RFC4122-compliant UUID.
mkUuidParser
  :: MonadParsec e s m
  => m (Token s)
  -- ^ Hex-digit parser.
  -> m (Token s)
  -- ^ Dash parser.
  -> (Token s -> Char)
  -- ^ Convert a token to a 'Char'.
  -> m UUID
mkUuidParser hexDigitChar dash toChar = do
  part1 <- replicateM 8 hexDigitChar <* dash
  part2 <- replicateM 4 hexDigitChar <* dash
  part3 <- replicateM 4 hexDigitChar <* dash
  part4 <- replicateM 4 hexDigitChar <* dash
  part5 <- replicateM 12 hexDigitChar

  let toStr = map toChar

  pure
    ( fromJust
        ( U.fromString
            (intercalate "-" (map toStr [part1, part2, part3, part4, part5]))
        )
    )