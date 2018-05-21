{-# LANGUAGE OverloadedStrings #-}
module MyOAuthConfig where

import Google.OAuth
import Google.Drive

-- * My API settings
-- Read them at https://console.developers.google.com/apis/credentials?project=givorsdays
oauthConfig :: OAuthConfig
oauthConfig = OAuthConfig
  { clientID = ""
  , clientSecret = ""
  , scope = googleDriveScope
  }
