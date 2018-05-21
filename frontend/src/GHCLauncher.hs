{-# LANGUAGE OverloadedStrings #-}
module GHCLauncher where

import CSS

import Reflex.Dom.Core (mainWidgetWithCss)

import qualified Language.Javascript.JSaddle.Warp as JSaddle

import Settings

go :: IO ()
go = JSaddle.run 8081 (mainWidgetWithCss css settingsView)
