-- |
-- Module      : Text.Megaparsec.Utils
-- Description : Various generic parsers and combinators.
-- Copyright   : (c) drlkf, 2024
-- License     : GPL-3
-- Maintainer  : drlkf@drlkf.net
-- Stability   : experimental
--
-- Re-exports of 'Text.Megaparsec.Utils.Char' and 'Text.Megaparsec.Utils.Common',
-- plus 'String'-based conversion helpers. New code should use
-- 'Text.Megaparsec.Utils.Char' or 'Text.Megaparsec.Utils.Byte' directly.
module Text.Megaparsec.Utils (
  -- * Scalar parsers
  Char.boolParser,
  Char.numParser,
  Char.posDecNumParser,
  Char.posNumParser,
  Char.uuidParser,

  -- * Combinators
  Char.commaSeparated,
  Common.occurrence,
  Common.occurrences,

  -- * Compatibility utilities
  Char.boundedEnumShowParser,
  parsecToReadsPrec,
  parsecToJSONParser,
) where

import Data.Aeson.Types (Parser, Value, withText)
import qualified Data.Text as T (unpack)
import Text.Megaparsec (
  Parsec,
  ShowErrorComponent,
  errorBundlePretty,
  runParser,
 )
import qualified Text.Megaparsec.Utils.Char as Char
import qualified Text.Megaparsec.Utils.Common as Common

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
