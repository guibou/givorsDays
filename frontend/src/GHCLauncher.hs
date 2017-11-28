{-# LANGUAGE OverloadedStrings #-}
module GHCLauncher where

import Lib
import qualified Language.Javascript.JSaddle.Warp as JSaddle

run :: IO ()
run = JSaddle.run 8080 $ libMainWidget "http://192.168.1.21:8082"
