{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ExtendedDefaultRules, RecursiveDo, PartialTypeSignatures, DeriveAnyClass, DeriveGeneric, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards, TypeFamilies, TupleSections #-}

module Settings where

import Protolude
import Unsafe

import Reflex.Dom.Core

import qualified Data.ByteString.Lazy as ByteString.Lazy

import Data.Aeson

import Google.Drive
import Google.OAuth

import MyOAuthConfig
import Lib

import WebStorage

settingsView :: MonadWidget a m => m ()
settingsView = mdo
  currentStatus <- webStorageDyn @AppStatus "appStatus" (NotLogged Nothing) upStatus
  currentAccessToken <- current <$> webStorageDyn @AccessToken "accessToken" nullAccessToken (unsafeFromJust . getFirst <$> upAccessToken)

  currentCal <- webStorageDyn @Calendar "calendar" mempty upCal

  updateStatus <- dyn (runEventWriterT . widgetStatus currentAccessToken currentCal <$> currentStatus)
  upStatus <- switchHold never (fst <$> fst <$> updateStatus)
  upCal <- switchHold never (snd <$> fst <$> updateStatus)
  upAccessToken <- switchHold never (snd <$> updateStatus)

  pure ()

-- * TODO

filenamePattern :: Text
filenamePattern = "GivorsDays"

-- * Status / Widgets

data AppStatus
  = NotLogged (Maybe Text)
  | LoggedNoFile RefreshToken
  | LoggedWithFile Text RefreshToken
  deriving (Show, Generic, FromJSON, ToJSON)

widgetStatus :: _ => Behavior t AccessToken -> Dynamic t Calendar -> AppStatus -> EventWriterT t (First AccessToken) m (Event t AppStatus, Event t Calendar)
widgetStatus _ _ (NotLogged maybeCal) = do
    el "h1" $ text "Login phase"
    elAttr "a" ("href" =: urlAuth oauthConfig <> "target" =: "_blank") $ text "Click to log with Google"
    el "br" $ blank
    text "Copy the authentification token here: "
    loginKey <- textInput def
    loginEvnt <- button "LogIn"

    answer <- authenticate oauthConfig (current (value loginKey) `tag` loginEvnt)

    let
      displayStatus (Left err) = "Error: " <> show err
      displayStatus (Right _) = "OK"

      authEvent (Left _) = Nothing
      authEvent (Right (access_token, refresh_token)) = Just (access_token, nextStep)
        where
          nextStep = case maybeCal of
            Nothing -> LoggedNoFile refresh_token
            Just fileID -> LoggedWithFile fileID refresh_token

    el "br" $ blank

    textResult <- holdDyn "Nothing Yet" (displayStatus <$> answer)

    dynText textResult

    let loggedEvent = fmapMaybe authEvent answer
    tellEvent (First . Just . fst <$> loggedEvent)

    pure (snd <$> loggedEvent, never)
widgetStatus currentAccessToken _currentCal status@(LoggedNoFile refreshToken) = do
  el "h1" $ text "Google Drive File Selection"

  pb <- getPostBuild
  res <- performRequestAsyncWithRefreshToken oauthConfig refreshToken currentAccessToken (reqList filenamePattern <$ pb)

  let
   displayAFile (FileResponse{..}) = name <> ": " <> id

   toFileList :: MonadWidget t m => Either (Maybe Data.Aeson.Value) ListFilesResponse -> m (Event t Text)
   toFileList t = case t of
     Left err -> text ("Error: " <> show err) >> pure never
     Right lsFiles -> el "ul" $ do
       case files lsFiles of
         [] -> text "No files" >> pure never
         fs -> fmap leftmost (for fs $ \file@(FileResponse{..}) -> do
           el "li" $ do
             text (displayAFile file)
             bt <- button "Use that file"
             pure (bt $> id))

  let res' = toFileList <$> res

  evtSelectFile <- widgetHold (text "waiting for file list" >> pure never) res'
  evtSelectFile' <- switchHold never (updated evtSelectFile)

  text "selectedFile:"
  _ <- widgetHold (text "nop") (text <$> evtSelectFile')

  reqGetFileContent <- performRequestsAsyncWithRefreshToken oauthConfig refreshToken currentAccessToken ((\fileId -> (fileId, reqGet fileId)) <$> evtSelectFile')

  let
    fileEvent (_, (Left _)) = Nothing
    fileEvent (fileId, (Right _)) = Just (LoggedWithFile fileId refreshToken)

    calEvent (_, (Left _)) = Nothing
    calEvent (_fileId, (Right cal)) = Just cal

  let answerFileContent = reqGetFileContent

  el "br" $ blank

  c <- button "Create a file"
  resCreate <- performRequestAsyncWithRefreshToken @Data.Aeson.Value oauthConfig refreshToken currentAccessToken (reqCreateFile (mempty @Calendar) filenamePattern <$ c)

  logOut <- button "LogOut"

  pure $ (leftmost [
           resCreate $> status,
           logOut $> NotLogged Nothing,
           fmapMaybe fileEvent answerFileContent
           ],
           fmapMaybe calEvent answerFileContent)

widgetStatus currentAccessToken currentCal (LoggedWithFile fileId refreshToken) = do
  logOut <- button "LogOut"
  changeFile <- button "ChangeFile"

  updateCalEvent <- app currentCal

  -- Xhr request to update the calendar stored online
  _ <- performRequestAsyncWithRefreshToken @Data.Aeson.Value oauthConfig refreshToken currentAccessToken (reqUpdate fileId . ByteString.Lazy.toStrict . encode <$> updateCalEvent)

  pure $ (leftmost [
           changeFile $> (LoggedNoFile refreshToken),
           logOut $> NotLogged (Just fileId)
           ],
           updateCalEvent)

