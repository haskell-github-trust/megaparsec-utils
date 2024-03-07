{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Megaparsec.TimeSpec
  ( spec
  ) where

import           Control.Monad        (forM_)
import           Data.Either          (isLeft)
import           Data.List.Extra      (lower)
import           Data.Time            (DayOfWeek (..), TimeOfDay (..),
                                       defaultTimeLocale, formatTime)
import           Test.Hspec           (Spec, context, describe, it, shouldBe,
                                       shouldSatisfy)
import           Test.QuickCheck      (Arbitrary (..), elements, forAll,
                                       property, suchThat)
import           Text.Megaparsec      (runParser)
import           Text.Megaparsec.Time (dayParser, timeParser)
import           Text.Printf          (printf)

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay
    <$> ((`mod` 24) . abs <$> arbitrary)
    <*> ((`mod` 60) . abs <$> arbitrary)
    <*> pure 0

instance Arbitrary DayOfWeek where
  arbitrary = elements [Monday .. Sunday]

spec :: Spec
spec = do
  describe "day" $ do
    let weekday d = d < Saturday

    forM_
      [ ("full", "%A")
      , ("short", "%a")
      ] $ \(title, format) ->
      context title $ do
      it "nominal" . forAll (arbitrary `suchThat` weekday) $ \d ->
        runParser dayParser "day" (formatTime defaultTimeLocale format d) `shouldBe`
        Right (Right d)

      it "lowercase" . forAll (arbitrary `suchThat` weekday) $ \d ->
        runParser dayParser "day" (lower (formatTime defaultTimeLocale format d)) `shouldBe`
        Right (Right d)

      it "weekend" . forAll (arbitrary `suchThat` (not . weekday)) $ \d ->
        runParser dayParser "day" (formatTime defaultTimeLocale format d) `shouldSatisfy`
        isLeft

    context "tomorrow" $ do
      it "capitalized" $
        runParser dayParser "day" "Tomorrow "`shouldBe` Right (Left 1)

      it "lowercase" $
        runParser dayParser "day" "tomorrow "`shouldBe` Right (Left 1)

    it "future day" . forAll (abs <$> arbitrary) $ \x ->
      runParser dayParser "day" (printf "+%d" x) `shouldBe` Right (Left x)

    it "invalid" . forAll (negate . (+1) . abs <$> arbitrary) $ \x ->
      runParser dayParser "day" (printf "+%d" (x :: Int)) `shouldSatisfy` isLeft

  describe "time" $ do
    it "valid" . property $ \t ->
      runParser timeParser "time" (formatTime defaultTimeLocale "%R" t) `shouldBe`
      Right t
