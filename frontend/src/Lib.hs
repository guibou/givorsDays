{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# Language TemplateHaskell #-}

module Lib where

import Data.Traversable (for)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Functor (($>))
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Time (Day, addDays, utctDay, getCurrentTime)

import Reflex.Dom.Core

import Data.FileEmbed

import Date
import Utils
import WidgetMonth

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

loadCalendar :: MonadWidget t m
                   => m (Event t Calendar)
loadCalendar = do
  pb <- getPostBuild
  req <- getAndDecode (pb $> "http://localhost:8082/calendar")

  let result = fmapMaybe id req

  pure (Map.fromList <$> result)

toReq :: (Day, Int) -> Text
toReq (day, halfday) = "http://localhost:8082/update/" <> tShow day <> "/" <> tShow halfday

sendUpdates :: MonadWidget t m
            => Event t (Day, Int)
            -> m (Event t ())
sendUpdates e = do
  res <- getAndDecode (toReq <$> e)

  pure (fromMaybe () <$> res)

css = $(embedFile "app/default.css")

libMainWidget = mainWidgetWithCss css $ mdo
    currentMonth <- elClass "div" "header" $ do
      w <- monthSelectWidget
      el "span" $ dyn (makeResume <$> currentCal)
      pure w

    currentCal <- elClass "div" "calendar" $ mdo
      xhrCalendar <- loadCalendar

      let updatesEvents = updateCal <$> updates
      currentCal <- foldDyn ($) Map.empty (leftmost [const <$> xhrCalendar, updatesEvents])

      updates <- makeCalendar currentMonth currentCal

      _ <- sendUpdates updates

      pure currentCal
    blank


-- | Creates a calender
makeCalendar :: MonadWidget t m
             => Dynamic t CurrentMonth -- ^ The month to be displayed
             -> Dynamic t Calendar  -- ^ The current status of the days
             -> m (Event t (Day,Int))  -- ^ The updated days
makeCalendar dynCurrentMonth dynCalendar = mdo
  let startingDay = getStartingDay <$> dynCurrentMonth

  combos <- el "div" $
    el "table" $ do
      el "thead" $ el "tr" $ for_ daysList (el "th" . text)

      -- in the worse case, a 31 days month starting on sunday will span over 6 weeks
      el "tbody" $ for [0..5] $ \weekNo ->
          el "tr" $ for [0..6] $ \dayNo -> do
            let dayOffset = weekNo * 7 + dayNo
                currentDay = addDays dayOffset <$> startingDay

            calendarCell currentDay dynCalendar

  pure (leftmost (mconcat combos))

-- * Calendar Cell

dayClassName :: Int -> Text
dayClassName i = "day" <> tShow i

-- | Calendar cell widget
calendarCell :: MonadWidget t m
             => Dynamic t Day -- ^ The day to display
             -> Dynamic t Calendar -- ^ The current calendar (will be sampled to know the day value)
             -> m (Event t (Day, Int)) -- ^ Day modification event
calendarCell currentDay dynCalendar = mdo
  let
    currentValue' = readCal <$> dynCalendar <*> currentDay

  currentValue <- holdUniqDyn currentValue'

  -- the td class depends on the value of the comboBox, as dayX where X is the value of the combobox
  (tdClick, (value, event)) <- elDynClass' "td" (dayClassName  <$> value) $ mdo
    -- Name of the day, usually a number, but first of month (and first of year) are special
    res <- elClass "div" "combo" $ do
      widgetIntSelectorBox currentValue (domEvent Click tdClick)

    elClass "div" "dayName" $ dynText (getDayLabel <$> currentDay)

    pure res

  pure (attach (current currentDay) event)

widgetIntSelectorBox :: MonadWidget t m
                  => Dynamic t Int
                  -> Event t ()
                  -> m (Dynamic t Int, Event t Int)
widgetIntSelectorBox currentValue evtClick = mdo
  el "div" $ display value

  let
    setEvent = const <$> updated currentValue

    cycleI 4 = 0
    cycleI n = n + 1

    increaseEvent = evtClick $> cycleI

  value' <- foldDyn ($) 0 (leftmost [setEvent, increaseEvent])
  value <- holdUniqDyn value'

  pure (value, tag (cycleI <$> current value) increaseEvent)

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
