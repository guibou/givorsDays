{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards, DeriveAnyClass, PartialTypeSignatures, LambdaCase #-}

{-| OAuth token based authentification for Google services

- Use 'urlAuth' to open a google authentification page. You'll get a
  'Text' token to feed 'authenticate' and get your opaque 'AccessToken' and
  'RefreshToken'.
- use 'performRequestAsyncWithRefreshToken' and
  'performRequestsAsyncWithRefreshToken' to perform requests
-}

module Google.OAuth
  ( -- * Opaque token
    RefreshToken
  , AccessToken
  , OAuthConfig(..)
  , nullAccessToken
    -- * OAuth process
  , urlAuth
  , authenticate

    -- * Perform authentified requests
  , performRequestAsyncWithRefreshToken
  , performRequestsAsyncWithRefreshToken
  )


where

import GHC.Generics
import Data.Semigroup

import Data.Text (Text)
import Data.Aeson

import Reflex.Dom.Core

import Google.RequestUtils

-- * Token

-- | A Refresh token, used internally, you must save it (securly) if
--   you want to use the auto-refresh request features.
newtype RefreshToken = RefreshToken Text
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Access token used by many requests
newtype AccessToken = AccessToken Text
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | A null access token, it represents an unconnected status
nullAccessToken :: AccessToken
nullAccessToken = AccessToken ""

-- * Configuration

data OAuthConfig = OAuthConfig
  { clientID :: Text
  , clientSecret :: Text
  , scope :: Text
  } deriving (Show)

-- * Auth URL

redirectURICopyPaste :: Text
redirectURICopyPaste = "urn:ietf:wg:oauth:2.0:oob"

-- | Open this url to get a google OAuth authentification process
urlAuth :: OAuthConfig -> Text
urlAuth (OAuthConfig{..}) = buildUrl "https://accounts.google.com/o/oauth2/v2/auth" [
  ("client_id", clientID),
  ("redirect_uri", redirectURICopyPaste),
  ("response_type", "code"),
  ("scope", scope)]

-- *

data RefreshResponse = RefreshResponse
  { access_token :: Text
  , token_type :: Text
  , expires_in :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data TokenResponse = TokenResponse
  { access_token :: Text
  , token_type :: Text
  , expires_in :: Int
  , refresh_token :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

{- -- Not used for now
data InvalidCredentialsResponse = InvalidCredentialsResponse
  { error :: ErrorsCredentials
  , code :: Int
  , message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ErrorsCredentials = ErrorsCredentials
  { errors :: [ErrorCredentials]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ErrorCredentials = ErrorCredentials
  { domain :: Text
  , reason :: Text
  , message :: Text
  , locationType :: Text
  , location :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)
-}

-- * Request Token

requestAuthenticate :: OAuthConfig -> Text -> XhrRequest ()
requestAuthenticate (OAuthConfig{..}) authCode = xhrRequest "POST" ("https://www.googleapis.com/oauth2/v4/token?" <> payload) config
  where
    config = XhrRequestConfig headers Nothing Nothing Nothing () False AllHeaders
    headers = mempty
    payload = buildQueryString [
      ("code" ,  authCode),
        ( "client_id" ,  clientID),
        ( "client_secret" ,  clientSecret),
        ( "grant_type" ,  "authorization_code"),
        ( "redirect_uri" ,  redirectURICopyPaste)
      ]

authenticate :: _ => OAuthConfig -> Event t Text -> m (Event t (Either (Maybe Data.Aeson.Value) (AccessToken, RefreshToken)))
authenticate config e = do
  res <- fmap decodeXhrResponseNote <$> performRequestAsync (requestAuthenticate config <$> e)

  pure $ ffor res $ \case
    Left err -> Left err
    Right (TokenResponse{..}) -> Right (AccessToken access_token, RefreshToken refresh_token)

-- * Refresh Token

requestRefresh :: OAuthConfig -> RefreshToken -> XhrRequest ()
requestRefresh (OAuthConfig{..}) (RefreshToken refreshToken) = xhrRequest "POST" ("https://www.googleapis.com/oauth2/v4/token?" <> buildQueryString [
                                                ("refresh_token", refreshToken),
                                                ("client_id", clientID),
                                                ("client_secret", clientSecret),
                                                ("grant_type", "refresh_token")
                                                ]) (XhrRequestConfig mempty Nothing Nothing Nothing () False AllHeaders)


-- * Utils

-- * Perform requests

{- | Perform an async request using the 'AccessToken' stored in a behavior. The provided 'RefreshToken' will be used if the session expired.

     This functions lives in the 'EventWriterT' monad, meaning that you need to use them with 'runEventWriterT', such as:

     >>> (eventRequestResult, eventUpdateAccessToken) <- runEventWriterT (performRequestAsyncWithRefreshToken refreshToken behaviorAccessToken request)

     the `eventUpdateAccessToken` event fire with a new 'AccessToken', which you can use to update the 'Behavior' `behaviorAccessToken`.
-}
performRequestAsyncWithRefreshToken
  :: forall tResult a t m. _
  => OAuthConfig
  -> RefreshToken
  -> Behavior t AccessToken
  -> Event t (XhrRequest a) -- ^ The request
  -> m (Event t (Either (Maybe Data.Aeson.Value) tResult))
performRequestAsyncWithRefreshToken config tRefresh bAccess reqEvent = fmap snd <$> performRequestsAsyncWithRefreshToken config tRefresh bAccess ((\req -> ((), req)) <$> reqEvent)

-- | Same as 'performRequestsAsyncWithRefreshToken' but with a payload 'f'
performRequestsAsyncWithRefreshToken :: forall tResult a t m f. _ => OAuthConfig -> RefreshToken -> Behavior t AccessToken -> (Event t (f, XhrRequest a)) -> m (Event t ((f, (Either (Maybe Data.Aeson.Value) tResult))))
performRequestsAsyncWithRefreshToken config tRefresh bAccess reqEvent = do
  evt <- fmap (fmap decodeXhrResponseNote) <$>
    performRequestsAsync (attachWith (\tkn r -> let (payload, req) = r in ((payload, req), wrapRequest tkn req)) bAccess reqEvent)

  let
    fOk ((payload, _), Right r) = Just (payload, Right r)
    fOk (_, Left _) = Nothing

    fPasOk (_, Right _) = Nothing
    fPasOk (payload, Left _) = Just payload

    evtOk = fmapMaybe fOk evt
    evtPasOk = fmapMaybe fPasOk evt

  reqRefreshCredential <- fmap (fmap (decodeXhrResponseNote @RefreshResponse)) <$> performRequestsAsync ((\payload -> (payload, requestRefresh config tRefresh)) <$> evtPasOk)

  -- Refresh the token
  let
    fOkCredential (_, Left _) = Nothing
    fOkCredential ((payload, req), Right (RefreshResponse{..})) = Just ((payload, wrapRequest (AccessToken access_token) req), First (AccessToken access_token))

    okCredential = fmapMaybe fOkCredential reqRefreshCredential

  tellEvent (snd <$> okCredential)

  -- redo the request
  evt' <- fmap (fmap decodeXhrResponseNote) <$> performRequestsAsync (fst <$> okCredential)

  pure (leftmost [evtOk, evt'])


-- | Wrap a request to add OAuth token
wrapRequest :: AccessToken -> XhrRequest t -> XhrRequest t
wrapRequest (AccessToken tkn) req = req {_xhrRequest_config = (_xhrRequest_config req) { _xhrRequestConfig_headers = newHeaders }}
  where
    currentHeader = _xhrRequestConfig_headers (_xhrRequest_config req)
    newHeaders = currentHeader <> ("Authorization" =: ("Bearer " <> tkn))
