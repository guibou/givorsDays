{-# LANGUAGE OverloadedStrings #-}
module App where

import Protolude

import CSS

import Reflex.Dom (mainWidgetWithCss)
import Settings

go :: IO ()
go = mainWidgetWithCss css settingsView
