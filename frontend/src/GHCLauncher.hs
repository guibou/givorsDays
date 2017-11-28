{-# LANGUAGE OverloadedStrings #-}
module GHCLauncher where

import Lib
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import Data.Text (Text)

run :: Int -> Text -> IO ()
run port uri = JSaddle.run port $ libMainWidget uri
