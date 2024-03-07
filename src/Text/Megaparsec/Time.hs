{-# LANGUAGE FlexibleContexts #-}

module Text.Megaparsec.Time
  ( dayParser
  , timeParser
  ) where

import           Control.Monad             (replicateM, void)
import           Control.Monad.Combinators (choice, some)
import           Data.Functor              (($>))
import           Data.Time                 (DayOfWeek (..), TimeOfDay (..))
import           Data.Void                 (Void)
import           Text.Megaparsec           (Parsec, try)
import           Text.Megaparsec.Char      (char, digitChar, string')

dayParser :: Parsec Void String (Either Int DayOfWeek)
dayParser = choice
  [ Right <$> shortDay
  , Right <$> longDay
  , Left <$> (try (string' "tomorrow") $> 1)
  , Right <$> absoluteDay
  , Left <$> relativeDay
  ]
  where shortDay = choice $ map (ciString (take 3 . show)) weekDays
        longDay = choice $ map (ciString show) weekDays
        ciString f d = try (string' (f d)) $> d
        weekDays = [Monday .. Friday]
        absoluteDay = toEnum . read <$> try (some digitChar)
        relativeDay = char '+' >> read <$> try (some digitChar)

timeParser :: Parsec Void String TimeOfDay
timeParser = do
  h <- read <$> replicateM 2 digitChar
  void $ char ':'
  m <- read <$> replicateM 2 digitChar

  return $ TimeOfDay h m 0
