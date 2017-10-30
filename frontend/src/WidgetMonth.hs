{-# LANGUAGE OverloadedStrings #-}

module WidgetMonth where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)

import Data.Time (utctDay, getCurrentTime)

import Reflex.Dom.Core

import Date
import Utils

-- | A month selection widget composed of a year input and month input
monthSelectWidget :: MonadWidget t m
                  => m (Dynamic t CurrentMonth)
monthSelectWidget = do
  prev <- button "<"
  next <- button ">"

  (Day y m _) <- utctDay <$> liftIO getCurrentTime

  let startingMonth = CurrentMonth y m

  currentMonth <- foldDyn ($) startingMonth (leftmost
                                            [
                                              prevMonth <$ prev,
                                              nextMonth <$ next
                                            ])

  text " "
  dynText (formatMonth <$> currentMonth)
  text " "


  pure currentMonth

formatMonth (CurrentMonth y m) = monthsList !! (m - 1) <> " " <> tShow y
