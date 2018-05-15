{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Provides a left- or right- swip- Event for any dom item
--}
module Swip
  ( swipEvent
  , SwipDirection(..)
  , SwipConfig(..)
  )
  where
import Data.Time
import Reflex.Dom.Core
import Control.Monad.IO.Class

import Data.Default

-- * Swip config

data SwipConfig = SwipConfig
  { swipThresholdPx :: Int
  -- ^ Amount of pixels on left or right direction to trigger a swip event

  , swipThresholdTime :: NominalDiffTime
  -- ^ A swip event must not take long

  , swipDiagThreshold :: Int
  -- ^ A swip event must not go up or down of more than this threshold
  }
  deriving (Show)

instance Default SwipConfig where
  def = SwipConfig
    { swipThresholdPx = 50
    , swipThresholdTime = 0.2
    , swipDiagThreshold =  20
    }

-- * Swip Event handler

data SwipDirection = SwipLeft | SwipRight
  deriving (Show, Eq)

swipEvent :: _ => El a -> SwipConfig -> m (Event t SwipDirection)
swipEvent widget conf = do
  t0 <- tagTime (domEvent Touchstart widget)
  t1 <- tagTime (domEvent Touchend widget)
  d <- foldDyn (compactSwip conf) NoSwip (leftmost [
                             (StartSwip . unsafeExtractXY) <$> t0,
                             (EndSwip . unsafeExtractXY) <$> t1
                             ])

  pure $ fforMaybe (updated d) $ \e -> case e of
    OkSwip s -> Just s
    _ -> Nothing


-- * Utils

-- | Tag and event with the current time
tagTime :: _ => Event a t -> m (Event a (t, UTCTime))
tagTime e = performEvent $ ffor e $ \v -> do
  t <- liftIO getCurrentTime
  pure (v, t)

-- | Extract screen x and screen y component of a TouchEventResult
unsafeExtractXY :: (TouchEventResult, t) -> (t, Int, Int)
unsafeExtractXY (e, time) = let
  ct = _touchEventResult_changedTouches e
  at = head ct
  in (time, _touchResult_screenX at, _touchResult_screenY at)

data SwipStatus
  = StartSwip (UTCTime, Int, Int)
  | EndSwip (UTCTime, Int, Int)
  deriving (Show)

data SwipStatusBis
  = RunningSwip (UTCTime, Int, Int)
  | OkSwip SwipDirection
  | NoSwip
  deriving (Show)

compactSwip :: SwipConfig -> SwipStatus -> SwipStatusBis -> SwipStatusBis
compactSwip _ (StartSwip v) _ = RunningSwip v
compactSwip (SwipConfig{..}) (EndSwip (te, xe, ye)) (RunningSwip (ts, xs, ys))
  | let dir = xs - xe
  , te `diffUTCTime` ts < swipThresholdTime
  , abs (ye - ys) < swipDiagThreshold
  , abs dir > swipThresholdPx
  = if dir < 0
    then OkSwip SwipRight
    else OkSwip SwipLeft
compactSwip _ _ _ = NoSwip
