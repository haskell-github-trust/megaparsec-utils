{-# LANGUAGE FlexibleContexts #-}

module Text.Megaparsec.Time
  ( dateParser
  , dayParser
  , durationParser
  , gregorianDayParser
  , hoursParser
  , minutesParser
  , secondsParser
  , timeParser
  ) where

import           Control.Applicative       (optional, (<|>))
import           Control.Monad             (replicateM, void)
import           Control.Monad.Combinators (choice, some)
import           Data.Functor              (($>))
import           Data.List.Extra           (lower)
import           Data.Maybe                (fromMaybe)
import           Data.Time                 (Day, DayOfWeek (..),
                                            NominalDiffTime, TimeOfDay (..),
                                            defaultTimeLocale, parseTimeM,
                                            secondsToNominalDiffTime)
import           Text.Megaparsec           (Parsec, takeRest, try)
import           Text.Megaparsec.Char      (char, digitChar, space, space1,
                                            string')
import           Text.Megaparsec.Utils     (posNumParser)

type DayResult = Either Int DayOfWeek

dateParser
  :: Ord e
  => Parsec e String (Maybe DayResult, TimeOfDay)
dateParser = (,) <$> optional (try (dayParser <* space1)) <*> timeParser

dayParser
  :: Ord e
  => Parsec e String DayResult
dayParser = choice
  [ Right <$> longDay
  , Right <$> shortDay
  , Left  <$> (string' "yesterday" $> -1)
  , Left  <$> (string' "tomorrow" $> 1)
  , Right <$> absoluteDay
  , Left  <$> relativeDay
  ] where shortDay = choice $ map (ciString (lower . take 3 . show)) weekDays
          longDay  = choice $ map (ciString (lower . show)) weekDays
          ciString f d = try (string' (f d)) $> d
          weekDays = [Monday .. Friday]
          sign = (char '-' $> negate) <|> (char '+' $> id)
          absoluteDay = toEnum . read <$> some digitChar
          relativeDay = ($) <$> sign <*> (read <$> some digitChar)

durationParser
  :: Ord e
  => Parsec e String NominalDiffTime
durationParser = try hours <|> try minutes <|> secondsParser
  where hours = do
          h <- hoursParser <* space
          m <- fromMaybe zero <$> optional (try minutes)
          s <- fromMaybe zero <$> optional secondsParser

          return $ h + m + s

        minutes = do
          m <- minutesParser <* space
          s <- fromMaybe zero <$> optional secondsParser

          return $ m + s

gregorianDayParser
  :: Ord e
  => Parsec e String Day
gregorianDayParser = do
  s <- takeRest
  parseTimeM False defaultTimeLocale "%F" s <|>
    parseTimeM False defaultTimeLocale "%d/%m/%Y" s

hoursParser
  :: Ord e
  => Parsec e String NominalDiffTime
hoursParser = secondsToNominalDiffTime . (* 3600) <$> posNumParser <* char 'h'

minutesParser
  :: Ord e
  => Parsec e String NominalDiffTime
minutesParser = secondsToNominalDiffTime . (* 60) <$> posNumParser <* char 'm'

secondsParser
  :: Ord e
  => Parsec e String NominalDiffTime
secondsParser = secondsToNominalDiffTime <$> posNumParser <* optional (char 's')

timeParser
  :: Ord e
  => Parsec e String TimeOfDay
timeParser = do
  h <- read <$> replicateM 2 digitChar
  void $ char ':'
  m <- read <$> replicateM 2 digitChar

  return $ TimeOfDay h m 0

zero :: NominalDiffTime
zero = secondsToNominalDiffTime 0
