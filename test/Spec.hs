{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Data.Aeson                 (FromJSON (..), ToJSON,
                                             Value (Object), defaultOptions,
                                             genericToEncoding, toEncoding,
                                             (.:))
import           Data.Aeson.Types           (typeMismatch)
import           Data.Text                  (Text)
import qualified Data.UUID                  as UUID
import           Data.UUID.V4               (nextRandom)
import           GHC.Generics               (Generic)
import           Test.Hspec

import           Audax.KVStore
import           Audax.KVStore.S3
import           Audax.Options
import           Audax.Prelude


data TestOptions
  = TestOptions
  { testOptionsCreds            :: AWSCredentials
  , testOptionsRegion           :: Region
  , testOptionsBucket           :: BucketName
  , testOptionsShouldRunS3Tests :: Bool
  } deriving (Generic)


instance Show TestOptions where
  show (TestOptions _ r b s3)
    = unwords [ "TestOptions"
              , "{"
              , "testOptionsCreds = {_},"
              , "testOptionsRegion ="
              , show r
              , ","
              , "testOptionsBucket ="
              , show b
              , ","
              , "testOptionsShouldRunS3Tests ="
              , show s3
              , "}"
              ]


instance FromJSON TestOptions where
  parseJSON (Object o)
    = TestOptions
      <$> o .: "credentials"
      <*> o .: "region"
      <*> o .: "bucket"
      <*> (o .: "shouldRunS3Tests" <|> return False)
  parseJSON v
    = typeMismatch "TestOptions" v


instance MonadIO m => MonadKVStore (ReaderT TestOptions m) where
  sendToKVStore t k v = do
    TestOptions c r b _ <- ask
    sendToS3KVStore c r b t k v
  readFromKVStore t k = do
    TestOptions c r b _ <- ask
    readFromS3KVStore c r b t k
  listKVStoreKeys t = do
    TestOptions c r b _ <- ask
    listFromS3KVStore c r b t Nothing
  deleteFromKVStore t k = do
    TestOptions c r b _ <- ask
    deleteFromS3KVStore c r b t k


data TestKVThing
  = TestKVThing
  { testKVThingName  :: Text
  , testKVThingCount :: Int
  } deriving (Generic, Show, Eq)


testKVThingPrefix :: Text
testKVThingPrefix = "tests"


instance FromJSON TestKVThing


instance ToJSON TestKVThing where
  toEncoding = genericToEncoding defaultOptions


instance ToKVKeys TestKVThing where
  toKVKeys (TestKVThing name _) = (testKVThingPrefix, name)


ensureSuccess
  :: ( MonadReader TestOptions m
     , MonadIO m
     )
  => Either String a
  -> m a
ensureSuccess (Left err) = do
  ask >>= liftIO . pPrint
  fail err
ensureSuccess (Right a)  = return a


canEncodeKVPair
  :: Text
  -> TestOptions
  -> IO ()
canEncodeKVPair name = runReaderT $ do
  let thing0 = TestKVThing name 0
  ensureSuccess
    =<< encodeToKVStoreAsKVPair thing0


canListEncodedPair
  :: Text
  -> TestOptions
  -> IO ()
canListEncodedPair name = runReaderT $ do
  keys <-
    ensureSuccess
      =<< listKVStoreKeys testKVThingPrefix
  liftIO $ keys `shouldSatisfy` any ((== name) . snd)


canReadKVPair
  :: Text
  -> TestOptions
  -> IO ()
canReadKVPair name = runReaderT
  $ void
  $ ensureSuccess
      =<< readFromKVStore testKVThingPrefix name


canDecodeKVPair
  :: Text
  -> TestOptions
  -> IO ()
canDecodeKVPair name = runReaderT $ do
  thing <-
    ensureSuccess
      =<< decodeFromKVStore testKVThingPrefix name
  liftIO $ thing `shouldSatisfy` (== TestKVThing name 0)


canDeleteKV
  :: Text
  -> TestOptions
  -> IO ()
canDeleteKV name = runReaderT
  $ ensureSuccess =<< deleteFromKVStore testKVThingPrefix name


main :: IO ()
main = do
  opts <- getYamlFile "cfg.yml"
  name <- UUID.toText <$> nextRandom
  hspec $ describe "KVStore" $ do
    it "should be able to encode to s3 as KV pair"
      $ canEncodeKVPair name opts

    it "should be able to list the encoded thing"
      $ canListEncodedPair name opts

    it "should be able to read the encoded thing"
      $ canReadKVPair name opts

    it "should be able to decode the encoded thing"
      $ canDecodeKVPair name opts

    it "should be able to delete the thing"
      $ canDeleteKV name opts
