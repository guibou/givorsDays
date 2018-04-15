{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Time
import           Control.Monad.IO.Class

import           Control.Exception (IOException, try)
import           Control.Error.Util
import qualified Data.ByteString.Lazy as BS
import           Control.Monad (forever)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Gzip
import           Network.Socket (SockAddr)
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import Data.Char (ord, chr)

-- * app
logger req s mi = do
  print(req, s, mi)

gzipSettings = def { gzipFiles = GzipPreCompressed (GzipCacheFolder "/tmp/cacheGzip") }

run :: String -> Int -> IO ()
run filename port = do
  let settings =
        setLogger logger $
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp filename

readDatabase :: BS.ByteString -> Calendar
readDatabase s = toCal (mapMaybe decode' (BS.split (fromIntegral (ord '\n')) s))
  where
    toCal :: [LogLine] -> Calendar
    -- We use the right biased behavior of Map.fromList to keep only the last value
    toCal l = Calendar $ Map.fromList (map (\(LogLine day n _) -> (day, n)) l)

mkApp :: String -> IO Application
mkApp filename = do
  calendarS <- try @IOException $ BS.readFile filename
  let cal = case calendarS of
        Left _ -> Calendar (Map.empty)
        Right s -> readDatabase s

  calVar <- newMVar cal

  -- serve the API
  let api = getCalendar calVar :<|> getMonth calVar :<|> updateCalendar filename calVar :<|> serveDirectoryFileServer "static"
  return $ gzip gzipSettings $ simpleCors (serve (Proxy @CalendarApi) api)

-- * api

type CalendarApi =
  "calendar" :> Get '[JSON] [(Day, Int)] :<|>
  "month" :> Capture "year" Integer :> Capture "month" Int :> Get '[JSON] [(Day, Int)] :<|>
  "update" :> Capture "day" Day :> Capture "halfdays" Int :> RemoteHost :> Get '[JSON] () :<|>
  Raw


-- * endpoints
data Calendar = Calendar (Map.Map Day Int)
  deriving (Generic, ToJSON, FromJSON, Show)

getCalendar :: MVar Calendar -> Handler [(Day, Int)]
getCalendar c = do
  Calendar cal <- liftIO (readMVar c)
  pure (Map.toList cal)

getMonth :: MVar Calendar -> Integer -> Int -> Handler [(Day, Int)]
getMonth c year month = do
  Calendar cal <- liftIO (readMVar c)

  let list = Map.toList cal
      ret = filter (\(d, _) -> let (y, m, _) = toGregorian d in y == year && m == month) list
  pure ret

updateCalendar :: String -> MVar Calendar -> Day -> Int -> SockAddr -> Handler ()
updateCalendar filePath c day n ip = do
  t <- liftIO $ getCurrentTime
  liftIO $ modifyMVar_ c $ \cal -> do
    let newCal = updateDay day n cal

    -- save the db atomically
    withFile filePath AppendMode $ \fp -> do
      BS.hPutStr fp ((encode (LogLine day n (show ip <> " " <> show t))) <> "\n")

    pure newCal

-- * model

updateDay day 0 (Calendar cal) = Calendar $ Map.delete day cal
updateDay day n (Calendar cal) = Calendar $ Map.insert day n cal

-- Do not change this API, the json is dumped to the file
data LogLine = LogLine
  { logLineDay :: Day -- ^ The updated day
  , logLineNumber :: Int -- ^ The worked number of half days [0-4]
  , logLineInfo :: String -- ^ A log line in any stupid format
  }
  deriving (Show, Generic, ToJSON, FromJSON)
