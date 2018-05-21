{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeApplications, ExtendedDefaultRules, RecursiveDo, PartialTypeSignatures, DeriveAnyClass, DeriveGeneric, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}
module WebStorage
  (webStorageDyn)
where

import Data.Text (Text)
import Reflex.Dom.Core
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString

import Language.Javascript.JSaddle
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Language.Javascript.JSaddle.Object (jsg)

import Data.Maybe (fromMaybe)

-- * Web Storage

-- | @@webStorageDyn name def evt@@ returns a 'Dynamic' which initial value is read from the webstorage field `name`, or from `def` and updated by 'evt'
webStorageDyn :: forall t a m. _ => Text -> t -> Event a t -> m (Dynamic a t)
webStorageDyn name initVal evtUpdate = do
  initialValue <- webStorageInit name
  performEvent_ (saveWebStorage name <$> evtUpdate)
  holdDyn (fromMaybe initVal initialValue) evtUpdate

webStorageInit :: forall t m. (MonadJSM m, FromJSON t) => Text -> m (Maybe t)
webStorageInit name = liftJSM $ do
    jsVal <- jsg @Text "localStorage" >>= (! name)
    v <- fromJSVal @Text jsVal

    case v of
      Nothing -> pure Nothing
      Just t -> pure (decodeStrict (encodeUtf8 t))

saveWebStorage :: (ToJSON t, MonadJSM m) => Text -> t -> m ()
saveWebStorage name v = liftJSM $ do
  storage <- jsg @Text "localStorage"

  encodedVal <- toJSVal @Text (decodeUtf8 (ByteString.toStrict (encode v)))

  (storage <# name) encodedVal
