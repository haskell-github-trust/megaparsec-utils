{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Megaparsec.UtilsSpec
  ( spec
  ) where

import           Control.Applicative             (some)
import           Control.Applicative.Combinators (choice)
import           Control.Monad                   (void)
import           Data.Char                       (isAlphaNum, toUpper)
import           Data.Either                     (isLeft)
import           Data.List                       (intercalate)
import           Data.List.NonEmpty              (NonEmpty ((:|)))
import qualified Data.List.NonEmpty              as N (toList)
import           Data.Void                       (Void)
import           Test.Hspec                      (Expectation, Spec, SpecWith,
                                                  context, describe, it,
                                                  shouldBe, shouldSatisfy)
import           Test.QuickCheck                 (Arbitrary (..), Gen, elements,
                                                  forAll, listOf, listOf1,
                                                  property, suchThat)
import           Text.Megaparsec                 (Parsec, eof, parseMaybe,
                                                  runParser)
import           Text.Megaparsec.Char            (alphaNumChar, char, digitChar,
                                                  string)
import           Text.Megaparsec.Utils           (boundedEnumShowParser,
                                                  commaSeparated, numParser,
                                                  occurrence, occurrences,
                                                  posDecNumParser, posNumParser)
import           Text.Printf                     (printf)

newtype SomeData = SomeData Int
  deriving Eq

instance Show SomeData where
  show (SomeData i) = show i

instance Arbitrary SomeData where
  arbitrary = SomeData . abs <$> arbitrary

someDataParser :: Parsec Void String SomeData
someDataParser = SomeData . read <$> some digitChar

data SomeEnum
  = SomeA
  | SomeB
  | SomeC
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary SomeEnum where
  arbitrary = elements [SomeA, SomeB, SomeC]

showableParser :: Show a => a -> Parsec Void String a
showableParser a = string (show a) >> pure a

someEnumParser :: Parsec Void String SomeEnum
someEnumParser = choice $ map showableParser [(minBound :: SomeEnum) ..]

data SomeADT = SomeADT
  { _id   :: Int
  , _name :: String
  , _type :: SomeEnum
  } deriving Eq

instance Show SomeADT where
  show (SomeADT i n t) = intercalate "," [show i, n, show t]

instance Arbitrary SomeADT where
  arbitrary = SomeADT . abs
    <$> arbitrary
    <*> listOf1 (arbitrary `suchThat` isAlphaNum)
    <*> arbitrary

someADTParser :: Parsec Void String SomeADT
someADTParser = do
  i <- read <$> some digitChar
  void $ char ','
  n <- some alphaNumChar
  void $ char ','
  SomeADT i n <$> someEnumParser

input :: Arbitrary a => Gen (String, a, String)
input = (,,)
  <$> listOf (arbitrary `suchThat` flip notElem forbiddenChars)
  <*> arbitrary
  <*> listOf (arbitrary `suchThat` flip notElem forbiddenChars)
  where forbiddenChars = ['0'..'9'] ++ concatMap show [(minBound :: SomeEnum) ..]

exhaustive
  :: Show a
  => Enum a
  => Bounded a
  => (a -> Expectation)
  -> SpecWith ()
exhaustive f = foldl1 (>>) $ mkIt <$> values
  where mkIt v = it (pad (show v)) $ f v
        padNum = foldr (max . length . show) 0 values
        pad s = s ++ replicate (padNum - length s) ' '
        values = [minBound..]

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "parsers" $ do
    it "SomeData" . property $ \v ->
      parseMaybe someDataParser (show (v :: SomeData)) `shouldBe` Just v

    it "SomeEnum" . property $ \v ->
      parseMaybe someEnumParser (show (v :: SomeEnum)) `shouldBe` Just v

    it "SomeADT" . property $ \v ->
      parseMaybe someADTParser (show (v :: SomeADT)) `shouldBe` Just v

    context "posDecNumParser" $ do
      it "no decimals" . property $ \v ->
        parseMaybe posDecNumParser (show (abs (v :: Int))) `shouldBe`
        Just (fromIntegral (abs v))

      it "decimals" . property $ \v ->
        parseMaybe posDecNumParser (printf "%f" (abs (v :: Double))) `shouldBe`
        Just (abs v)

    it "posNumParser" . property $ \v ->
      parseMaybe posNumParser (show (abs (v :: Int))) `shouldBe` Just (abs v)

    it "numParser" . property $ \v ->
      parseMaybe numParser (show (v :: Int)) `shouldBe` Just v

  describe "boundedEnumShowParser" $ do
    context "lowercase" . exhaustive $ \v ->
      parseMaybe (boundedEnumShowParser <* eof) (show v) `shouldBe` Just (v :: SomeEnum)

    context "uppercase" . exhaustive $ \v ->
      parseMaybe (boundedEnumShowParser <* eof) (map toUpper (show v))
      `shouldBe` Just (v :: SomeEnum)

    context "mixed" . exhaustive $ \v -> do
      let capitalize i x | even i    = toUpper x
                         | otherwise = x
          mixCase = zipWith capitalize [(0 :: Int) ..]
      parseMaybe (boundedEnumShowParser <* eof) (mixCase (show v))
        `shouldBe` Just (v :: SomeEnum)

  describe "occurrence" $ do
    it "SomeData" . forAll input $ \(prefix, v, suffix) -> do
      let s = unwords [prefix, show (v :: SomeData), suffix]
      runParser (occurrence someDataParser) "test" s `shouldBe` Right v

    it "SomeEnum" . forAll input $ \(prefix, v, suffix) -> do
      let s = unwords [prefix, show (v :: SomeEnum), suffix]
      runParser (occurrence someEnumParser) "test" s `shouldBe` Right v

    it "SomeADT" . forAll input $ \(prefix, v, suffix) -> do
      let s = unwords [prefix, show (v :: SomeADT), suffix]
      runParser (occurrence someADTParser) "test" s `shouldBe` Right v

  describe "occurrences" $ do
    it "SomeData" . forAll input $ \(prefix, v, suffix) -> do
      let s = unwords [prefix, show (v :: SomeData), suffix]
      runParser (occurrences someDataParser) "test" s `shouldBe` Right [v]

    it "SomeEnum" . forAll input $ \(prefix, v, suffix) -> do
      let s = unwords [prefix, show (v :: SomeEnum), suffix]
      runParser (occurrences someEnumParser) "test" s `shouldBe` Right [v]

    it "SomeADT" . forAll input $ \(prefix, v, suffix) -> do
      let s = unwords [prefix, show (v :: SomeADT), suffix]
      runParser (occurrences someADTParser) "test" s `shouldBe` Right [v]

  describe "comma-separated" $ do
    context "valid" $ do
      it "single" . property $ \x -> do
        let y = abs x
        runParser (commaSeparated numParser) "test" (show y)
          `shouldBe` Right (y :| [])

      it "multiple" . property $ \xs -> do
        let ys = fmap abs xs
            s = intercalate "," . map show $ N.toList ys
        runParser (commaSeparated numParser) "test" s
          `shouldBe` Right ys

    context "invalid" $ do
      it "empty" $
        runParser (commaSeparated numParser) "test" "" `shouldSatisfy` isLeft

      it "first" $
        runParser (commaSeparated numParser) "test" "abc,42" `shouldSatisfy` isLeft

      it "first partially correct" $
        runParser (commaSeparated (numParser <* eof)) "test" "42abc,42"
        `shouldSatisfy` isLeft

      it "second" $
        runParser (commaSeparated numParser) "test" "42,abc" `shouldSatisfy` isLeft
