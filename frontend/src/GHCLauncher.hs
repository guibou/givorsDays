{-# LANGUAGE OverloadedStrings #-}
module GHCLauncher where

import Lib
import CSS

import Reflex.Dom.Core

import qualified Language.Javascript.JSaddle.Warp as JSaddle

run :: IO ()
run = JSaddle.run 8081 (mainWidgetWithCss css app)
