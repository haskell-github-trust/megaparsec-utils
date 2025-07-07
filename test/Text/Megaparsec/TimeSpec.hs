{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Megaparsec.TimeSpec (
  spec,
) where

import Control.Monad (forM_)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (toLower)
import Data.Either (isLeft)
import Data.Time (
  Day,
  DayOfWeek (..),
  TimeOfDay (..),
  defaultTimeLocale,
  formatTime,
  fromGregorian,
  secondsToNominalDiffTime,
 )
import Data.Void (Void)
import Test.Hspec (
  Spec,
  context,
  describe,
  it,
  shouldBe,
  shouldSatisfy,
 )
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  elements,
  forAll,
  property,
  suchThat,
 )
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Time (
  dateParser,
  dayParser,
  durationParser,
  gregorianDayParser,
  hoursParser,
  minutesParser,
  secondsParser,
  timeParser,
 )
import Text.Printf (printf)

instance Arbitrary TimeOfDay where
  arbitrary =
    ( TimeOfDay . (`mod` 24) . abs
        <$> arbitrary
    )
      <*> ((`mod` 60) . abs <$> arbitrary)
      <*> pure 0

instance Arbitrary DayOfWeek where
  arbitrary = elements [Monday .. Sunday]

instance Arbitrary Day where
  arbitrary =
    fromGregorian
      <$> elements [1970 .. 2100]
      <*> elements [1 .. 12]
      <*> arbitrary

positive
  :: Num a
  => Arbitrary a
  => Gen a
positive = abs <$> arbitrary

parseOrPrettyError
  :: Parsec Void String a
  -> String
  -> Either String a
parseOrPrettyError p = first errorBundlePretty . runParser p "test"

spec :: Spec
spec = do
  describe "date" $ do
    it "time only" $
      parseOrPrettyError dateParser "11:03"
        `shouldBe` Right (Nothing, TimeOfDay 11 3 0)

    context "with day" $ do
      it "short" $
        parseOrPrettyError dateParser "tue 11:03"
          `shouldBe` Right (Just (Right Tuesday), TimeOfDay 11 3 0)

      it "long" $
        parseOrPrettyError dateParser "tuesday 11:03"
          `shouldBe` Right (Just (Right Tuesday), TimeOfDay 11 3 0)

    context "with rel day" $ do
      it "positive" $
        parseOrPrettyError dateParser "+1 11:03"
          `shouldBe` Right (Just (Left 1), TimeOfDay 11 3 0)

      it "negative" $
        parseOrPrettyError dateParser "-1 11:03"
          `shouldBe` Right (Just (Left (-1)), TimeOfDay 11 3 0)

    it "with yesterday" $
      parseOrPrettyError dateParser "yesterday 11:03"
        `shouldBe` Right (Just (Left (-1)), TimeOfDay 11 3 0)

  describe "day" $ do
    let weekday d = d < Saturday

    forM_
      [ ("full", "%A")
      , ("short", "%a")
      ]
      $ \(title, format) ->
        context title $ do
          it "nominal" . forAll (arbitrary `suchThat` weekday) $ \d ->
            parseOrPrettyError dayParser (formatTime defaultTimeLocale format d)
              `shouldBe` Right (Right d)

          it "lowercase" . forAll (arbitrary `suchThat` weekday) $ \d ->
            parseOrPrettyError
              dayParser
              (fmap toLower (formatTime defaultTimeLocale format d))
              `shouldBe` Right (Right d)

          it "weekend" . forAll (arbitrary `suchThat` (not . weekday)) $ \d ->
            parseOrPrettyError dayParser (formatTime defaultTimeLocale format d)
              `shouldSatisfy` isLeft

    context "tomorrow" $ do
      it "capitalized" $
        parseOrPrettyError dayParser "Tomorrow" `shouldBe` Right (Left 1)

      it "lowercase" $
        parseOrPrettyError dayParser "tomorrow" `shouldBe` Right (Left 1)

    it "future day" . forAll (abs <$> arbitrary) $ \x ->
      parseOrPrettyError dayParser (printf "+%d" x) `shouldBe` Right (Left x)

    it "invalid" . forAll (negate . (+ 1) . abs <$> arbitrary) $ \x ->
      parseOrPrettyError dayParser (printf "+%d" (x :: Int)) `shouldSatisfy` isLeft

  describe "duration" $ do
    it "hours" . forAll positive $ \h ->
      parseOrPrettyError hoursParser (printf "%dh" h)
        `shouldBe` Right (secondsToNominalDiffTime (fromInteger h * 3600))

    it "minutes" . forAll positive $ \m ->
      parseOrPrettyError minutesParser (printf "%dm" m)
        `shouldBe` Right (secondsToNominalDiffTime (fromInteger m * 60))

    context "seconds" $ do
      it "raw" . forAll positive $ \s ->
        parseOrPrettyError secondsParser (printf "%d" s)
          `shouldBe` Right (secondsToNominalDiffTime (fromInteger s))

      it "with suffix" . forAll positive $ \s ->
        parseOrPrettyError secondsParser (printf "%ds" s)
          `shouldBe` Right (secondsToNominalDiffTime (fromInteger s))

    context "duration" $ do
      context "hours" $ do
        it "hms" . forAll ((,,) <$> positive <*> positive <*> positive) $ \(h, m, s) ->
          parseOrPrettyError durationParser (printf "%dh %dm %ds" h m s)
            `shouldBe` Right (secondsToNominalDiffTime (fromInteger (((h * 60 + m) * 60) + s)))

        it "hm" . forAll ((,) <$> positive <*> positive) $ \(h, m) ->
          parseOrPrettyError durationParser (printf "%dh %dm" h m)
            `shouldBe` Right (secondsToNominalDiffTime (fromInteger ((h * 60 + m) * 60)))

        it "hs" . forAll ((,) <$> positive <*> positive) $ \(h, s) ->
          parseOrPrettyError durationParser (printf "%dh %ds" h s)
            `shouldBe` Right (secondsToNominalDiffTime (fromInteger (h * 3600 + s)))

      context "minutes" $ do
        it "ms" . forAll ((,) <$> positive <*> positive) $ \(m, s) ->
          parseOrPrettyError durationParser (printf "%dm%ds" m s)
            `shouldBe` Right (secondsToNominalDiffTime (fromInteger (m * 60 + s)))

        it "m" . forAll positive $ \m ->
          parseOrPrettyError durationParser (printf "%dm" m)
            `shouldBe` Right (secondsToNominalDiffTime (fromInteger (m * 60)))

      context "seconds" $ do
        it "s" . forAll positive $ \s ->
          parseOrPrettyError durationParser (printf "%ds" s)
            `shouldBe` Right (secondsToNominalDiffTime (fromInteger s))

        it "no suffix" . forAll positive $ \s ->
          parseOrPrettyError durationParser (show s)
            `shouldBe` Right (secondsToNominalDiffTime (fromInteger s))

  describe "gregorian day" $ do
    forM_ ["%F", "%d/%m/%Y"] $ \format ->
      it format . property $ \d ->
        parseOrPrettyError gregorianDayParser (formatTime defaultTimeLocale format d)
          `shouldBe` Right d

  describe "time" $ do
    it "invalid hours" . forAll (arbitrary `suchThat` (\h -> h > 23 && h < 100)) $ \h ->
      parseOrPrettyError timeParser (printf "%02d:00" (h :: Int)) `shouldSatisfy` isLeft

    it "invalid hours" . forAll (arbitrary `suchThat` (\m -> m > 59 && m < 100)) $ \m ->
      parseOrPrettyError timeParser (printf "00:%02d" (m :: Int)) `shouldSatisfy` isLeft

    it "valid" . property $ \t ->
      parseOrPrettyError timeParser (formatTime defaultTimeLocale "%R" t)
        `shouldBe` Right t
