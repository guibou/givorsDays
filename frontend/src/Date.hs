{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Date where

import Protolude
import Unsafe

import qualified Data.Text as Text
import Data.Text (Text)

import Data.Time (fromGregorian, toGregorian, Day)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate

-- | French listing of month names
monthsList :: [Text]
monthsList = Text.words "Janvier Fevrier Mars Avril Mai Juin Juillet Aout Septembre Octobre Novembre Decembre"
-- monthsList = map (Text.take 3) $ Text.words "Janvier Février Mars Avril Mai Juin Juillet Août Septembre Octobre Novembre Decembre"

-- | French listing of day names
daysList :: [Text]
daysList = map (Text.take 3) $ Text.words "Lundi Mardi Mercredi Jeudi Vendredi Samedi Dimanche"

-- | Convert between gregorian and *Day*
pattern Day :: Integer -> Int -> Int -> Day
pattern Day y m d <- (toGregorian -> (y, m, d))
  where Day y m d = fromGregorian y m d

{-# COMPLETE Day #-}

{- | Returns the label for a Month

     There is a special case for first of month / year:

     >>> getMonthLabel (Month 2017 5 23)

     >>> getMonthLabel (Month 2017 5 1)
     Mai
     >>> getMonthLabel (Month 2017 1 1)
     Janvier 2017
-}
getMonthLabel :: Day -> Text
getMonthLabel (Day y m d) = ifMonth <> ifYear
  where ifMonth = if d == 1
                  then " " <> monthsList `unsafeIndex` (m - 1)
                  else ""
        ifYear = if m == 1 && d == 1
                 then " " <> show y
                 else ""

-- | Returns the first Monday before the begining of this month
--   i.e. if your month starts on Wednesday, this functions returns a day 2 days earlier
getStartingDay :: CurrentMonth -> (Day, Int)
getStartingDay (CurrentMonth year month) =
  let
    leapYear = isLeapYear year
    mLength = monthLength leapYear month

    (startingYear, startingWeek, weekDayNo) = toWeekDate (fromGregorian year month 1)
    startingDay = fromWeekDate startingYear startingWeek 1
  in (startingDay, 1 + (weekDayNo - 1 + mLength - 1) `div` 7)

data CurrentMonth = CurrentMonth Integer Int deriving (Show)

prevMonth :: CurrentMonth -> CurrentMonth
prevMonth (CurrentMonth y 1) = CurrentMonth (y - 1) 12
prevMonth (CurrentMonth y m) = CurrentMonth y (m - 1)

nextMonth :: CurrentMonth -> CurrentMonth
nextMonth (CurrentMonth y 12) = CurrentMonth (y + 1) 1
nextMonth (CurrentMonth y m) = CurrentMonth y (m + 1)
