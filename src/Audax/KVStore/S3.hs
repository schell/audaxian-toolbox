-- | Provides bolt on pretty logging.
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Audax.KVStore.S3
  ( module Audax.KVStore.S3
  , BucketName
  , Region
  ) where

import           Control.Lens                 (set, view, (&), (.~), (<&>),
                                               (?~), (^.))
import           Control.Monad                (join)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.AWS      (AWST', Credentials (..), Env,
                                               LogLevel (..), ToBody (..),
                                               envLogger, envRegion, newEnv,
                                               newLogger, runAWST, send)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types             (typeMismatch)
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.Binary          as CB
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Network.AWS                  (sinkBody)
import           Network.AWS.S3               (BucketName, ObjectKey (..),
                                               Region, deleteObject,
                                               dorsResponseStatus, getObject,
                                               gorsBody, gorsResponseStatus,
                                               listObjectsV2,
                                               lovContinuationToken, lovPrefix,
                                               lovrsContents, lovrsIsTruncated,
                                               lovrsNextContinuationToken, oKey,
                                               porsResponseStatus, putObject)
import           System.IO                    (stdout)
import           Text.Show.Pretty             (ppShow)
import           UnliftIO.Exception           (catchAny)


newtype AWSCredentials = AWSCredentials { unAWSCredentials :: Credentials }


instance Show AWSCredentials where
  show (AWSCredentials creds) = "AWSCredentials \"" ++ show creds ++ "\""


instance FromJSON AWSCredentials where
  parseJSON (Object o) = do
    keys <- FromKeys
      <$> o .: "accessKey"
      <*> o .: "accessSecret"
    return $ AWSCredentials keys
  parseJSON (String "discover") = pure $ AWSCredentials Discover
  parseJSON v = typeMismatch "Credentials" v


runS3
  :: Show b
  => AWSCredentials
  -> Region
  -> AWST' Env (ResourceT IO) b
  -> IO (Either String b)
runS3 creds r f = do
  let creds1 = unAWSCredentials creds
  lgr <- newLogger Info stdout
  env <- liftIO $
    newEnv creds1
    <&> set envRegion r
      . set envLogger lgr
  catchAny
    (fmap Right $ runResourceT $ runAWST env f)
    (return . Left . show)


sendToS3KVStore
  :: ( MonadIO m
     , ToBody a
     )
  => AWSCredentials
  -> Region
  -> BucketName
  -> Text
  -> Text
  -> a
  -> m (Either String ())
sendToS3KVStore c r b t k v = do
  ebody <- liftIO
    $ runS3 c r
    $ send
    $ putObject
        b
        (ObjectKey $ T.concat [t, "/", k])
        (toBody v)
  return $ case ebody of
    Left err  -> Left err
    Right rsp ->
      case rsp ^. porsResponseStatus of
        200 -> Right ()
        201 -> Right ()
        _   -> Left $ ppShow r


readFromS3KVStore
  :: ( MonadIO m )
  => AWSCredentials
  -> Region
  -> BucketName
  -> Text
  -> Text
  -> m (Either String BL.ByteString)
readFromS3KVStore c r b t k = do
  ee <- liftIO
    $ runS3 c r
    $ do
      rsp <- send
        $ getObject
            b
            (ObjectKey $ T.concat [t, "/", k])
      if rsp ^. gorsResponseStatus `elem` [200, 201]
        then Right <$> view gorsBody rsp `sinkBody` CB.sinkLbs
        else return $ Left $ ppShow rsp
  return $ join ee


deleteFromS3KVStore
  :: ( MonadIO m )
  => AWSCredentials
  -> Region
  -> BucketName
  -> Text
  -> Text
  -> m (Either String ())
deleteFromS3KVStore c r b t k = do
  ebody <- liftIO
    $ runS3 c r
    $ send
    $ deleteObject
        b
        (ObjectKey $ T.concat [t, "/", k])
  case ebody of
    Left err -> return $ Left err
    Right rsp ->
      return $ case rsp ^. dorsResponseStatus of
        200 -> Right ()
        201 -> Right ()
        204 -> Right ()
        _   -> Left $ ppShow r


-- | Turn an S3 ObjectKey into a KVStore keypair.
--
-- >>> toKeypairs (ObjectKey "dir1/file.txt")
-- ( "dir1" , "file.txt" )
toKeypairs
  :: ObjectKey
  -> (Text, Text)
toKeypairs (ObjectKey k) =
  let (a, b) = T.breakOn "/" k
  in (a, T.drop 1 b)


-- | List the objects in the bucket with the given prefix.
-- A continuation token is used to get all the keys from s3.
listFromS3KVStore
  :: ( MonadIO m )
  => AWSCredentials
  -> Region
  -> BucketName
  -> Text
  -- ^ The key prefix
  -> Maybe Text
  -- ^ The continuation token
  -> m (Either String [(Text, Text)])
listFromS3KVStore c r b prefix mayToken = do
  er <- liftIO
    $ runS3 c r
    $ send
    $ listObjectsV2 b
    & lovPrefix            ?~ prefix
    & lovContinuationToken .~ mayToken

  case er of
    Left err    -> return $ Left err
    Right lovrs -> do

      let keys = map (toKeypairs . (^. oKey)) (lovrs ^. lovrsContents)

      if lovrs ^. lovrsIsTruncated == Just True
      then do
        er1 <- listFromS3KVStore c r b prefix (lovrs ^. lovrsNextContinuationToken)
        return $ (keys ++) <$> er1
      else return $ Right keys
