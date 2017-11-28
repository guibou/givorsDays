module Main where

import System.Environment (getArgs)

import Lib

main :: IO ()
main = do
  args <- getArgs
  let filename = args !! 0
      port = read (args !! 1)

  run filename port
