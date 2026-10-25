{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Text.Megaparsec.Utils.Byte
-- Description : Parsers for streams of 'Word8'.
-- Copyright   : (c) drlkf, 2026
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Generic parsers for streams of 'Word8' (strict and lazy 'ByteString').
module Text.Megaparsec.Utils.Byte (
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
) where

import Data.List.NonEmpty (NonEmpty)
import Data.UUID (UUID)
import Data.Word (Word8)
import Text.Megaparsec (MonadParsec, Token)
import Text.Megaparsec.Byte (char, char', digitChar, hexDigitChar)
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
boolParser
  :: MonadParsec e s m
  => Token s ~ Word8
  => m Bool
boolParser = mkBoolParser char' (toEnum . fromEnum)

-- | Parse a 'Bounded' 'Enum' type that has a 'Show' instance, trying all
-- possibilities, case-insensitive, in the 'Enum' order.
boundedEnumShowParser
  :: forall a e s m
   . MonadParsec e s m
  => Token s ~ Word8
  => Bounded a
  => Enum a
  => Show a
  => m a
boundedEnumShowParser = mkBoundedEnumShowParser char' (toEnum . fromEnum)

-- | Parse a comma-separated list of items.
commaSeparated
  :: MonadParsec e s m
  => Token s ~ Word8
  => m a
  -> m (NonEmpty a)
commaSeparated = mkCommaSeparated (char 44)

-- | Parse a positive number, with or without decimals prefixed by a @.@.
posDecNumParser
  :: MonadFail m
  => MonadParsec e s m
  => Read a
  => Token s ~ Word8
  => m a
posDecNumParser = mkPosDecNumParser digitChar (char 46) (toEnum . fromIntegral)

-- | Parse a positive integer.
posNumParser
  :: MonadFail m
  => MonadParsec e s m
  => Read a
  => Token s ~ Word8
  => m a
posNumParser = mkPosNumParser digitChar (toEnum . fromIntegral)

-- | Parse an integer, without any space between minus sign and digits.
numParser
  :: MonadFail m
  => MonadParsec e s m
  => Num a
  => Read a
  => Token s ~ Word8
  => m a
numParser = mkNumParser (char 45) posNumParser

-- | Parse a RFC4122-compliant UUID.
uuidParser
  :: MonadParsec e s m
  => Token s ~ Word8
  => m UUID
uuidParser = mkUuidParser hexDigitChar (char 45) (toEnum . fromIntegral)
