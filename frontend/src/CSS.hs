{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay
import Clay.Filter

import Data.Monoid ((<>))
import Data.ByteString (ByteString)

css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  table ? do
    borderCollapse collapse
    "table-layout" -: "fixed"
  td ? do
    position relative
    width (pct 14.28)
    border solid (px 1) lightgrey
    margin (0 :: Size LengthUnit) 0 0 0
    userSelect none

    "div.dayName" ? do
      fontSize (em 1.6)
      textAlign center
    "div.monthName" ? do
      position absolute
      top (px 5)
      left (px 5)
      right (px 0)
      fontSize (em 1.6)
      textAlign center
      color darkslateblue
      overflow hidden

  ".day1" ? backgroundColor lightgreen
  ".day2" ? backgroundColor yellow
  ".day3" ? backgroundColor orangered
  ".day4" ? backgroundColor red

  ".outOfMonth" ? Clay.filter (Clay.Filter.opacity (pct 30))
  ".outOfMonth .monthName" ? Clay.filter (Clay.Filter.opacity (pct 200))

  html <> body ? do
    width (pct 100)
    height (pct 100)
    margin (0 :: Size LengthUnit) 0 0 0
    padding (0 :: Size LengthUnit) 0 0 0

  ".calendar" ? do
    position absolute
    top (em 2.7)
    bottom (0 :: Size LengthUnit)
    left (0 :: Size LengthUnit)
    right (em 0.5)
    margin (0 :: Size LengthUnit) 0 0 0
    padding (0 :: Size LengthUnit) 0 0 0

    -- fading effect when calendar change
    "td" ? do
      animationName "expand"
      animationDuration (sec 0.5)

      keyframes "expand" [(0, Clay.opacity 0)]

  ".calendar" |> div ? do
    star <> table  <? do
      height (vh 100 @-@ em 2.7)
      width (vw 100 @-@ px 1)
      margin (0 :: Size LengthUnit) 0 0 0
      padding (0 :: Size LengthUnit) 0 0 0

  ".header" ? do
    fontSize (em 1.6)

  ".header button" ? do
    fontSize (em 1.2)

  ".header" ? span ? do
    textAlign (alignSide sideRight)
    float floatRight
    fontSize (em 0.7)
    paddingRight (px 5)
