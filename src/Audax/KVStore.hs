-- | A very simple key value storage layer abstraction.
-- See Audax.KVStore.S3 for implementation helpers backed by AWS S3.
{-# LANGUAGE MultiParamTypeClasses #-}
module Audax.KVStore where

import           Data.Aeson           (FromJSON (..), ToJSON (..), eitherDecode,
                                       encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)


-- | MTL style typeclass representing a synchronously accessible key-value store.
class MonadKVStore m where
  -- | Stores a value in a table at the given key.
  sendToKVStore
    :: Text
    -- ^ The first key
    -> Text
    -- ^ The second key
    -> ByteString
    -- ^ The serialized value
    -> m ()
  -- | Reads a value in a table at the given key.
  readFromKVStore
    :: Text
    -- ^ The first key
    -> Text
    -- ^ The second key
    -> m (Either String ByteString)
    -- ^ Possibly a value from a table by its key, or an error.
  -- | List the keys from the store that match the given prefix.
  listKVStoreKeys
    :: Text
    -- ^ The key prefix
    -> m (Either String [(Text, Text)])


class ToKVKeys a where
  toKVKeys :: a -> (Text, Text)


encodeToKVStore
  :: (MonadKVStore m, ToJSON a)
  => Text
  -> Text
  -> a
  -> m ()
encodeToKVStore t k = sendToKVStore t k . encode


encodeToKVStoreAsKVPair
  :: (MonadKVStore m, ToKVKeys a, ToJSON a)
  => a
  -> m ()
encodeToKVStoreAsKVPair a =
  let (t, k) = toKVKeys a
  in sendToKVStore t k $ encode a


decodeFromKVStore
  :: (MonadKVStore m, Monad m, FromJSON a)
  => Text
  -> Text
  -> m (Either String a)
decodeFromKVStore t k = do
  eBytes <- readFromKVStore t k
  return $ eBytes >>= eitherDecode
