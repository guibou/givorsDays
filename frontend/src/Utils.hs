module Utils where

import qualified Data.Text as Text
import Data.Text (Text)

import Data.List (groupBy)
import Data.Function (on)

toIntUnsafe :: Text -> Integer
toIntUnsafe = read . Text.unpack

tShow :: Show t => t -> Text
tShow = Text.pack . show

{- |
   >>> groupOn head ["aoeu", "aiii", "bou"]
   [('a', ["aoeu", "aiii"]), ('b', ["bou"])]
-}

groupOn :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn f l = let groups = groupBy (on (==) f) l
              in map (\group -> (f (head group), group)) groups
