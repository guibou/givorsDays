{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( Lib.run
    ) where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import qualified Data.Map as Map
import           Data.Time.Calendar
import           Control.Monad.IO.Class

import           Control.Exception (IOException, try)
import           Control.Error.Util
import qualified Data.ByteString.Lazy as BS
import           Control.Monad (forever)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           System.IO.SafeWrite

-- * app

run :: IO ()
run = do
  let port = 8082
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
  -- read the initial db content
  let filename = "saveFile"
  calendarS <- try @IOException $ BS.readFile filename
  let cal = case decode' =<< hush calendarS of
        Nothing -> Calendar (Map.empty)
        Just c -> c

  calVar <- newMVar cal

  -- serve the API
  let api = getCalendar calVar :<|> updateCalendar filename calVar
  return $ serve (Proxy @CalendarApi) api

-- * api

type CalendarApi =
  "calendar" :> Get '[JSON] Calendar :<|>
  "update" :> Capture "day" Day :> Capture "halfdays" Int :> Get '[JSON] ()

-- * endpoints

getCalendar :: MVar Calendar -> Handler Calendar
getCalendar c = do
  cal <- liftIO (readMVar c)
  pure cal

updateCalendar :: String -> MVar Calendar -> Day -> Int -> Handler ()
updateCalendar filePath c day n = do
  liftIO $ modifyMVar_ c $ \cal -> do
    let newCal = updateDay day n cal

    -- save the db atomically
    withOutputFile filePath $ \fp -> do
      BS.hPutStr fp (encode newCal)

    pure newCal

-- * model

data Calendar = Calendar (Map.Map Day Int)
  deriving (Generic, ToJSON, FromJSON, Show)

updateDay day 0 (Calendar cal) = Calendar $ Map.delete day cal
updateDay day n (Calendar cal) = Calendar $ Map.insert day n cal
