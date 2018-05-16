{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay

import Data.Monoid ((<>))
import Data.ByteString (ByteString)

css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  table ? borderCollapse collapse
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
      fontSize (em 1.6)
      textAlign center
      position absolute
      color lightblue
      top (px 0)
      zIndex (-1)

    "div.combo" ? do
      userSelect none
      fontSize (pct 80)

  ".day1" ? backgroundColor lightgreen
  ".day2" ? backgroundColor yellow
  ".day3" ? backgroundColor orangered
  ".day4" ? backgroundColor red

  html <> body ? do
    width (pct 100)
    height (pct 100)
    margin (0 :: Size LengthUnit) 0 0 0
    padding (0 :: Size LengthUnit) 0 0 0

  ".calendar" ? do
    position absolute
    top (em 1.5)
    bottom (0 :: Size LengthUnit)
    left (0 :: Size LengthUnit)
    right (em 0.5)
    margin (0 :: Size LengthUnit) 0 0 0
    padding (0 :: Size LengthUnit) 0 0 0

  ".calendar" |> div ? do
    star <> table  <? do
      height (vh 100 @-@ em 1.6)
      width (vw 100 @-@ px 1) -- 1 px, because ?
      margin (0 :: Size LengthUnit) 0 0 0
      padding (0 :: Size LengthUnit) 0 0 0

  ".header" ? span ? do
    textAlign (alignSide sideRight)
    float floatRight
