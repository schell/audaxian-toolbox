-- | Provides bolt on pretty logging.
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Audax.KVStore.S3 where

import           Control.Lens                 (set, view, (<&>))
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.AWS      (AWST', Credentials (..), Env,
                                               ToBody (..), envRegion, newEnv,
                                               runAWST, send)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson                   (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types             (typeMismatch)
import           Network.AWS                  (sinkBody)
import           Network.AWS.S3               (BucketName, ObjectKey (..),
                                               Region, getObject, gorsBody,
                                               putObject)
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.Binary          as CB
import           Data.Text                    (Text)
import qualified Data.Text                    as T
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
  :: AWSCredentials
  -> Region
  -> AWST' Env (ResourceT IO) b
  -> IO (Either String b)
runS3 creds r f = do
  let creds1 = unAWSCredentials creds
  env <- liftIO $
    newEnv creds1
    <&> set envRegion r
    --    . set envLogger lgr
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
  -> m ()
sendToS3KVStore c r b t k v = do
  void
    $ liftIO
    $ runS3 c r
    $ send
    $ putObject
        b
        (ObjectKey $ T.concat [t, "/", k])
        (toBody v)


readFromS3KVStore
  :: ( MonadIO m )
  => AWSCredentials
  -> Region
  -> BucketName
  -> Text
  -> Text
  -> m (Either String BL.ByteString)
readFromS3KVStore c r b t k = do
  ebody <- liftIO
    $ runS3 c r
    $ send
    $ getObject
        b
        (ObjectKey $ T.concat [t, "/", k])
  case ebody of
    Left err   -> return $ Left err
    Right body -> Right <$> view gorsBody body `sinkBody` CB.sinkLbs
