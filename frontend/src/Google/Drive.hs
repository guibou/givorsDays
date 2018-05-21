{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, DeriveGeneric, GeneralizedNewtypeDeriving, DuplicateRecordFields, DeriveAnyClass #-}
module Google.Drive where

import Reflex.Dom.Core
import Data.Semigroup
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString.Lazy (toStrict)
import GHC.Generics

import Data.Text.Encoding (encodeUtf8)

import Google.RequestUtils

import Data.ByteString (ByteString)

-- * Second part, get the token

data ListFilesResponse = ListFilesResponse
  { kind :: Text
  , incompleteSearch :: Bool
  , files :: [FileResponse]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data FileResponse = FileResponse
  { kind :: Text
  , id :: Text
  , name :: Text
  , mimeType :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | List the files matching a pattern
reqList :: Text -> XhrRequest ()
reqList filenamePattern = xhrRequest "GET" ("https://www.googleapis.com/drive/v3/files?" <> buildQueryString [
  ("q", "name='" <> filenamePattern <> "'")
  ]) (XhrRequestConfig mempty Nothing Nothing Nothing () False AllHeaders)

-- | Get the body of a file by ID
reqGet :: Text -> XhrRequest ()
reqGet fileID = xhrRequest "GET" ("https://www.googleapis.com/drive/v3/files/" <> fileID <> "?alt=media") (XhrRequestConfig mempty Nothing Nothing Nothing () False AllHeaders)

-- | Update a file by ID
reqUpdate :: Text -> ByteString -> XhrRequest ByteString
reqUpdate fileID newContent = xhrRequest "PATCH" ("https://www.googleapis.com/upload/drive/v3/files/" <> fileID <> "?uploadType=media") (XhrRequestConfig mempty Nothing Nothing Nothing newContent False AllHeaders)

-- | Create a file
reqCreateFile :: ToJSON t => t -> Text -> XhrRequest ByteString
reqCreateFile content filenamePattern = xhrRequest "POST" ("https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart") (XhrRequestConfig headers Nothing Nothing Nothing mparts False AllHeaders)
  where
    headers = "Content-Type" =: "multipart/related; boundary=foo_bar_baz"
              <> "Content-Length" =: (Text.pack (show (ByteString.length mparts)))

    mparts :: ByteString
    mparts = multiPart "foo_bar_baz" [
      ("application/json;charset=UTF-8", "{'name' : '" <> encodeUtf8 filenamePattern <> "', 'mimeType' : 'text/plain'}")
      , ("text/plain",(toStrict $ encode content))
      ]

-- * Utils

multiPart :: ByteString -> [(ByteString, ByteString)] -> ByteString
multiPart boundary items = theBoundary <> ByteString.intercalate theBoundary (map toPart items) <> theBoundary <> "--"
  where
    theBoundary = "\n--" <> boundary <> "\n"
    toPart (contentType, content) = "Content-Type: " <> contentType <> "\n\n" <> content

-- * scope

googleDriveScope :: Text
googleDriveScope = "https://www.googleapis.com/auth/drive"
