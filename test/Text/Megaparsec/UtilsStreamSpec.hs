{-# LANGUAGE TypeApplications #-}

module Text.Megaparsec.UtilsStreamSpec (
  spec,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)
import qualified Text.Megaparsec.Utils.Byte as Byte
import qualified Text.Megaparsec.Utils.Char as Char
import Text.Megaparsec.Utils.Common (occurrence)

parseText :: Parsec Void Text a -> Text -> Either (ParseErrorBundle Text Void) a
parseText p = runParser p "test"

parseBytes
  :: Parsec Void ByteString a
  -> ByteString
  -> Either (ParseErrorBundle ByteString Void) a
parseBytes p = runParser p "test"

charNumParser :: Parsec Void Text Int
charNumParser = Char.numParser

byteNumParser :: Parsec Void ByteString Int
byteNumParser = Byte.numParser

spec :: Spec
spec = do
  describe "Char parsers on Text" $ do
    it "numParser" . property $ \v ->
      parseText charNumParser (T.pack (show (v :: Int))) `shouldBe` Right v

    it "boolParser" $ do
      parseText Char.boolParser (T.pack "yes") `shouldBe` Right True
      parseText Char.boolParser (T.pack "OFF") `shouldBe` Right False

    it "uuidParser" $ do
      parseText Char.uuidParser (T.pack "123e4567-e89b-12d3-a456-426614174000")
        `shouldBe` Right (read "123e4567-e89b-12d3-a456-426614174000")

    it "occurrence" . property $ \v ->
      parseText (occurrence charNumParser) (T.pack ("abc " <> show (v :: Int)))
        `shouldBe` Right v

  describe "Byte parsers on ByteString" $ do
    it "numParser" . property $ \v ->
      parseBytes byteNumParser (B.pack (show (v :: Int))) `shouldBe` Right v

    it "boolParser" $ do
      parseBytes Byte.boolParser (B.pack "yes") `shouldBe` Right True
      parseBytes Byte.boolParser (B.pack "OFF") `shouldBe` Right False

    it "uuidParser" $ do
      parseBytes Byte.uuidParser (B.pack "123e4567-e89b-12d3-a456-426614174000")
        `shouldBe` Right (read "123e4567-e89b-12d3-a456-426614174000")

    it "occurrence" . property $ \v ->
      parseBytes (occurrence byteNumParser) (B.pack ("abc " <> show (v :: Int)))
        `shouldBe` Right v
