{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# LANGUAGE CPP #-}

module Lib where

import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Functor (($>))

import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Time (Day, addDays)

#ifdef USE_WARP
import Reflex.Dom.Core
#else
import Reflex.Dom
#endif

import Date
import Utils
import WidgetMonth
import CSS
import WebStorage
import Swip

-- * Intro

{- |
This is a dynamic calendar where each day can be associated a value in
[0, 4], which are the number of half day worked that day. Yes, we can work 4 half days in a day ;)
-}

-- * Model

type Calendar = Map Day Int

-- A Calendar implictly associat 0 to each day
-- the *updateCal* compress the calendar by removing zeroed keys
-- the *readCal* returns the 0 if needed

updateCal :: (Day, Int) -> Calendar -> Calendar
updateCal (day, 0) cal = Map.delete day cal
updateCal (day, n) cal = Map.insert day n cal

readCal :: Calendar -> Day -> Int
readCal calendar day = fromMaybe 0 $ Map.lookup day calendar

libMainWidget = mainWidgetWithCss css $ mdo
    currentCal <- webStorageDyn @Calendar "calendar" mempty (attachWith (flip updateCal) (current currentCal) updates)

    swipE <- swipEvent elCalendar def

    currentMonth <- elClass "div" "header" $ do
      w <- monthSelectWidget (ffilter (==SwipLeft) swipE $> ()) (ffilter (==SwipRight) swipE $> ())
      _ <- el "span" $ dyn (makeResume <$> currentCal)
      pure w

    (elCalendar, updates) <- elClass' "div" "calendar" $ mdo
      pb <- getPostBuild
      let updateEvent = leftmost [pb, updated currentMonth $> ()]

      let dynCalendar  = makeCalendar <$> currentMonth <*> currentCal
      let evtCalendar = tagPromptlyDyn dynCalendar updateEvent

      updates <- widgetHold (text "loading" >> pure never) evtCalendar

      pure (switchPromptlyDyn updates)

    blank

-- | Creates a calender
makeCalendar :: MonadWidget t m
             => CurrentMonth -- ^ The month to be displayed
             -> Calendar  -- ^ The current status of the days
             -> m (Event t (Day,Int))  -- ^ The updated days
makeCalendar currentMonth calendar = mdo
  let
    (startingDay, nbWeeks) = getStartingDay currentMonth

  combos <- el "div" $
    el "table" $ do
      el "thead" $ el "tr" $ for_ daysList (el "th" . text)

      -- in the worse case, a 31 days month starting on sunday will span over 6 weeks
      el "tbody" $ for [0.. (nbWeeks - 1)] $ \weekNo ->
          el "tr" $ for [0..6] $ \dayNo -> do
            let dayOffset = (fromIntegral weekNo) * 7 + dayNo
                currentDay = addDays dayOffset startingDay

            calendarCell currentDay (readCal calendar currentDay)

  pure (leftmost (mconcat combos))

-- * Calendar Cell

dayClassName :: Int -> Text
dayClassName i = "day" <> tShow i

-- | Calendar cell widget
calendarCell :: MonadWidget t m
             => Day -- ^ The day to display
             -> Int -- ^ The current value
             -> m (Event t (Day, Int)) -- ^ Day modification event
calendarCell currentDay@(Day _ _ d) initValue = mdo
  value <- cycle4 initValue (domEvent Click tdClick)
  (tdClick, _) <- elDynClass' "td" (dayClassName  <$> value) $ do
    elClass "div" "monthName" $ text (getMonthLabel currentDay)
    elClass "div" "dayName" $ text (tShow d)
    el "div" $ display value

  pure ((currentDay,) <$> updated value)

cycle4 :: MonadWidget t m
       => Int -- ^ Init value
       -> Event t () -- ^ Update event
       -> m (Dynamic t Int)
cycle4 initValue evtClick = do
  let
    cycleI 4 = 0
    cycleI n = n + 1

  foldDyn ($) initValue (cycleI <$ evtClick)

-- * Report

makeResume :: MonadWidget t m
           => Calendar
           -> m ()
makeResume calendar = do
  let items = Map.toAscList calendar

  for_ (groupOn ((\(Day y _ _) -> y) . fst) items) $ \(year, months) -> mdo
    let countYear = sum (map snd months)
    el "strong" $ text (tShow year)
    text (": " <> tShow countYear <> " ")
