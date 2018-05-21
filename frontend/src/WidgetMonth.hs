{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module WidgetMonth where

import Protolude
import Unsafe

import Data.Time (utctDay, getCurrentTime)

import Reflex.Dom.Core

import Date

-- | A month selection widget composed of a year input and month input
monthSelectWidget :: _
                  => Event t ()
                  -> Event t ()
                  -> m (Dynamic t CurrentMonth)
monthSelectWidget prevE nextE = do
  prev <- button "<"
  next <- button ">"

  (Day y m _) <- utctDay <$> liftIO getCurrentTime

  let startingMonth = CurrentMonth y m

  currentMonth <- foldDyn ($) startingMonth (leftmost
                                            [
                                              prevMonth <$ (leftmost [prev, prevE]),
                                              nextMonth <$ (leftmost [next, nextE])
                                            ])

  text " "
  dynText (formatMonth <$> currentMonth)
  text " "


  pure currentMonth

formatMonth :: CurrentMonth -> Text
formatMonth (CurrentMonth y m) = monthsList `unsafeIndex` (m - 1) <> " " <> show y
