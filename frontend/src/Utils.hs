module Utils where

import Protolude
import qualified Unsafe

import Data.List (groupBy)

{- |
   >>> groupOn head ["aoeu", "aiii", "bou"]
   [('a', ["aoeu", "aiii"]), ('b', ["bou"])]
-}

groupOn :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn f l = let groups = groupBy (on (==) f) l
              in map (\grp -> (f (Unsafe.unsafeHead grp), grp)) groups
