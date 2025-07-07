{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Text.Megaparsec.Time
Description : Various parsers for types related to time.
Copyright   : (c) drlkf, 2024
License     : GPL-3
Maintainer  : drlkf@drlkf.net
Stability   : experimental
-}

module Text.Megaparsec.Time
  ( DayResult

  , dateParser
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
import           Data.Char                 (toLower)
import           Data.Functor              (($>))
import           Data.Maybe                (fromMaybe)
import           Data.Time                 (Day, DayOfWeek (..),
                                            NominalDiffTime, TimeOfDay (..),
                                            defaultTimeLocale, parseTimeM,
                                            secondsToNominalDiffTime)
import           Text.Megaparsec           (Parsec, takeRest, try)
import           Text.Megaparsec.Char      (char, digitChar, space, space1,
                                            string')
import           Text.Megaparsec.Utils     (posNumParser)

-- | Representation of a parser result with either a number of days relative to
-- the current day, or a 'DayOfWeek'.
type DayResult = Either Int DayOfWeek

-- | Parse a tuple containing a day or not, and a 'TimeOfDay'.
dateParser
  :: Ord e
  => Parsec e String (Maybe DayResult, TimeOfDay)
dateParser = (,) <$> optional (try (dayParser <* space1)) <*> timeParser

-- | Parse a day using one of the following, all case-insensitive:
--
--   * a short (3-letters) or long day name e.g @mon@ or @monday@
--   * @yesterday@ or @tomorrow@
--   * a day number relative to the current day i.e @+2@ is two days from today
--   * an absolute number for a 'DayOfWeek', refer to its 'Num' instance for more information.
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
  ] where shortDay = choice $ map (ciString (fmap toLower . take 3 . show)) weekDays
          longDay  = choice $ map (ciString (fmap toLower . show)) weekDays
          ciString f d = try (string' (f d)) $> d
          weekDays = [Monday .. Friday]
          sign = (char '-' $> negate) <|> (char '+' $> id)
          absoluteDay = toEnum . read <$> some digitChar
          relativeDay = ($) <$> sign <*> (read <$> some digitChar)

-- | Parse a 'NominalDiffTime' using strings like @1h23m45s@, with all
-- components being optional as long as one is present.
durationParser
  :: Ord e
  => Parsec e String NominalDiffTime
durationParser = try hours <|> try minutes <|> secondsParser
  where hours = do
          h <- hoursParser <* space
          m <- fromMaybe zero <$> optional (try minutes)
          s <- fromMaybe zero <$> optional secondsParser

          return (h + m + s)

        minutes = do
          m <- minutesParser <* space
          s <- fromMaybe zero <$> optional secondsParser

          return (m + s)

-- | Parse a Gregorian 'Day' from a @%d\/%m\/%Y@ format.
gregorianDayParser
  :: Ord e
  => Parsec e String Day
gregorianDayParser = do
  s <- takeRest
  parseTimeM False defaultTimeLocale "%F" s <|>
    parseTimeM False defaultTimeLocale "%d/%m/%Y" s

-- | Parse a 'NominalDiffTime' from a number of hours from a string like @1h@.
hoursParser
  :: Ord e
  => Parsec e String NominalDiffTime
hoursParser = secondsToNominalDiffTime . (* 3600) <$> posNumParser <* char 'h'

-- | Parse a 'NominalDiffTime' from a number of minutes from a string like @1m@.
minutesParser
  :: Ord e
  => Parsec e String NominalDiffTime
minutesParser = secondsToNominalDiffTime . (* 60) <$> posNumParser <* char 'm'

-- | Parse a 'NominalDiffTime' from a number of seconds from a string like @1s@.
secondsParser
  :: Ord e
  => Parsec e String NominalDiffTime
secondsParser = secondsToNominalDiffTime <$> posNumParser <* optional (char 's')

-- | Parse a 'TimeOfDay' from a string like @01:23@.
timeParser
  :: Ord e
  => Parsec e String TimeOfDay
timeParser = do
  h <- read <$> replicateM 2 digitChar <* char ':'
  m <- read <$> replicateM 2 digitChar

  return $ TimeOfDay h m 0

-- | Zero seconds in 'NominalDiffTime'.
zero :: NominalDiffTime
zero = secondsToNominalDiffTime 0
