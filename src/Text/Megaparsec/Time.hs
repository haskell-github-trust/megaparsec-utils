{-# LANGUAGE FlexibleContexts #-}

module Text.Megaparsec.Time
  ( dateParser
  , dayParser
  , durationParser
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
import           Data.Time                 (DayOfWeek (..), NominalDiffTime,
                                            TimeOfDay (..),
                                            secondsToNominalDiffTime)
import           Data.Void                 (Void)
import           Text.Megaparsec           (Parsec, try)
import           Text.Megaparsec.Char      (char, digitChar, space, space1,
                                            string')
import           Text.Megaparsec.Utils     (posNumParser)

type DayResult = Either Int DayOfWeek

dateParser :: Parsec Void String (Maybe DayResult, TimeOfDay)
dateParser = (,) <$> optional (try (dayParser <* space1)) <*> timeParser

dayParser :: Parsec Void String DayResult
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

durationParser :: Parsec Void String NominalDiffTime
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

hoursParser :: Parsec Void String NominalDiffTime
hoursParser = secondsToNominalDiffTime . (* 3600) <$> posNumParser <* char 'h'

minutesParser :: Parsec Void String NominalDiffTime
minutesParser = secondsToNominalDiffTime . (* 60) <$> posNumParser <* char 'm'

secondsParser :: Parsec Void String NominalDiffTime
secondsParser = secondsToNominalDiffTime <$> posNumParser <* optional (char 's')

timeParser :: Parsec Void String TimeOfDay
timeParser = do
  h <- read <$> replicateM 2 digitChar
  void $ char ':'
  m <- read <$> replicateM 2 digitChar

  return $ TimeOfDay h m 0

zero :: NominalDiffTime
zero = secondsToNominalDiffTime 0
