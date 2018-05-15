{-# LANGUAGE OverloadedStrings #-}
module GHCLauncher where

import Lib
import qualified Language.Javascript.JSaddle.Warp as JSaddle

run :: Int -> IO ()
run port = JSaddle.run port libMainWidget
