{-# LANGUAGE OverloadedStrings #-}
module Google.RequestUtils where

import Protolude

import qualified Data.Text as Text
import Reflex.Dom
import Data.Aeson

buildQueryString :: [(Text, Text)] -> Text
buildQueryString l = Text.intercalate "&" (map (\(a, b) -> a <> "=" <> b) l)

buildUrl :: Text -> [(Text, Text)] -> Text
buildUrl s [] = s
buildUrl s l = s <> "?" <> buildQueryString l

decodeXhrResponseNote :: FromJSON t => XhrResponse -> Either (Maybe Data.Aeson.Value) t
decodeXhrResponseNote r = case decodeXhrResponse r of
  Just v -> Right v
  Nothing -> Left $ case decodeXhrResponse r of
    Just v -> Just v
    Nothing -> Nothing
