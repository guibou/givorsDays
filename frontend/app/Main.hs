{-# LANGUAGE OverloadedStrings #-}

-- This module is only used for android compilation
import Settings
import CSS

import Reflex.Dom

main :: IO ()
main = mainWidgetWithCss css settingsView
