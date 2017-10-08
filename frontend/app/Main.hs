{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Traversable (for)
import Data.Foldable (for_)
import Data.List (groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Functor (($>))

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Time (fromGregorian, toGregorian, Day, addDays)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)

import Reflex.Dom

-- * Intro

{- |
This is a dynamic calendar where each day can be associated a value in
[0, 4], which are the number of half day worked that day. Yes, we can work 4 half days in a day ;)
-}

-- * Some constants and utils

-- | French listing of month names
monthsList :: [Text]
monthsList = Text.words "Janvier Février Mars Avril Mai Juin Juillet Août Septembre Octobre Novembre Decembre"

-- | French listing of day names
daysList :: [Text]
daysList = Text.words "Lundi Mardi Mercredi Jeudi Vendredi Samedi Dimanche"

-- | Convert between gregorian and *Day*
pattern Day :: Integer -> Int -> Int -> Day
pattern Day y m d <- (toGregorian -> (y, m, d))
  where Day y m d = fromGregorian y m d

{-# COMPLETE Day #-}

{- | Returns the label for a Day

     There is a special case for first of month / year:

     >>> getDayLabel (Day 2017 5 23)
     23
     >>> getDayLabel (Day 2017 5 1)
     1 Mai
     >>> getDayLabel (Day 2017 1 1)
     1 Janvier 2017
-}
getDayLabel :: Day -> Text
getDayLabel (Day y m d) = tShow d <> ifMonth <> ifYear
  where ifMonth = if d == 1
                  then " " <> (monthsList !! (m - 1))
                  else ""
        ifYear = if m == 1 && d == 1
                 then " " <> tShow y
                 else ""
toIntUnsafe :: Text -> Integer
toIntUnsafe = read . Text.unpack

tShow :: Show t => t -> Text
tShow = Text.pack . show

{- |
   >>> groupOn head ["aoeu", "aiii", "bou"]
   [('a', ["aoeu", "aiii"]), ('b', ["bou"])]
-}

groupOn :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn f l = let groups = groupBy (on (==) f) l
              in map (\group -> (f (head group), group)) groups

-- * Model

data CurrentMonth = CurrentMonth Integer Int
type Calendar = Map Day Int

-- A Calendar implictly associat 0 to each day
-- the *updateCal* compress the calendar by removing zeroed keys
-- the *readCal* returns the 0 if needed

updateCal :: (Day, Int) -> Calendar -> Calendar
updateCal (day, 0) cal = Map.delete day cal
updateCal (day, n) cal = Map.insert day n cal

readCal :: Calendar -> Day -> Int
readCal calendar day = fromMaybe 0 $ Map.lookup day calendar

-- For test, initial value, will be loaded by XHR request
initCalendar :: Calendar
initCalendar = Map.singleton (fromGregorian 2017 10 1) 4

header :: MonadWidget t m => m ()
header = do
  el "title" $ text "Calendrier jours travaillés Givors"
  elAttr "link" (Map.fromList [("rel", "stylesheet"), ("type", "text/css"), ("href", "../default.css")]) blank


loadCalendar :: MonadWidget t m
                   => m (Event t Calendar)
loadCalendar = do
  pb <- getPostBuild
  req <- getAndDecode (pb $> "http://localhost:8082/calendar")

  let result = fmapMaybe id (traceEvent "CAL" req)

  pure (Map.fromList <$> result)

toReq :: (Day, Int) -> Text
toReq (day, halfday) = "http://localhost:8082/update/" <> tShow day <> "/" <> tShow halfday

sendUpdates :: MonadWidget t m
            => Event t (Day, Int)
            -> m (Event t ())
sendUpdates e = do
  res <- getAndDecode (toReq <$> e)

  pure (fromMaybe () <$> res)

main :: IO ()
main = mainWidgetWithHead header $
  el "div" $ mdo
    currentMonth <- el "h2" $ do
      text "Calendrier "
      monthSelectWidget

    -- Here there is kind of a dangerous loop. *currentCal* is input
    -- and output of *makeCalendar*
    currentCal <- foldDyn updateCal initCalendar updates
    updates <- makeCalendar currentMonth currentCal

    el "h2" $ text "Résumé"
    _ <- elClass "div" "report" $ dyn (makeResume <$> currentCal)

    blank

-- | A month selection widget composed of a year input and month input
monthSelectWidget :: MonadWidget t m
                  => m (Dynamic t CurrentMonth)
monthSelectWidget = do
  month <- dropdown 10 (constDyn $ Map.fromList (zip [1..] monthsList)) def
  text " "
  year <- textInput (def { _textInputConfig_inputType = "number", _textInputConfig_initialValue = "2017" })

  pure (CurrentMonth <$> (toIntUnsafe <$> value year) <*> value month)

-- * Calendar

-- | Returns the first Monday before the begining of this month
--   i.e. if your month starts on Wednesday, this functions returns a day 2 days earlier
getStartingDay :: CurrentMonth -> Day
getStartingDay (CurrentMonth year month) =
  let
    (startingYear, startingWeek, _) = toWeekDate (fromGregorian year month 1)
    startingDay = fromWeekDate startingYear startingWeek 1
  in startingDay

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
  initialValue <- readCal <$> sample (current dynCalendar) <*> sample (current currentDay)

  let
    currentValue = attachWith readCal (current dynCalendar) (updated currentDay)

  -- the td class depends on the value of the comboBox, as dayX where X is the value of the combobox
  dd <- elDynClass "td" (dayClassName <$> value dd) $ do
    -- Name of the day, usually a number, but first of month (and first of year) are special
    elClass "div" "dayName" $ dynText (getDayLabel <$> currentDay)

    elClass "div" "combo" $ do
      let possiblesValues = constDyn $ Map.fromList (map (\x -> (x, tShow x)) [0..4])
      dropdown initialValue possiblesValues (def { _dropdownConfig_setValue = currentValue })

  -- the change event will result in a calendar modification event
  pure (attach (current currentDay) (_dropdown_change dd))

-- * Report

makeResume :: MonadWidget t m
           => Calendar
           -> m ()
makeResume calendar = do
  let items = Map.toAscList calendar

  for_ (groupOn ((\(Day y _ _) -> y) . fst) items) $ \(year, months) -> mdo
    let countYear = sum (map snd months)
    el "h3" $ text (tShow year <> ": " <> tShow countYear)
    el "ul" $ for_ (groupOn ((\(Day _ m _) -> m) . fst) months) $ \(month, days) -> do
      let countMonth = sum (map snd days)
      el "li" $ text $ (monthsList !! (month - 1)) <> ": " <> tShow countMonth

