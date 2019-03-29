-- | The audaxian prelude. All modules importing this should set
-- -XNoImplicitPrelude.

{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Audax.Prelude
  ( module Audax.Prelude
  , module X
  ) where

-- Re-exports
import           Control.Applicative    as X (empty, (<|>))
import           Control.Concurrent     as X (threadDelay)
import           Control.Exception      (Exception, throwIO)
import           Control.Lens           as X (Lens', makeFields, makeLenses, to,
                                              view, (%~), (&), (.~), (<&>),
                                              (?~), (^.))
import           Control.Monad          as X (guard, msum, unless, when, (>=>))
import           Control.Monad.IO.Class as X (MonadIO (..))
import           Control.Monad.Logger   as X (LogLevel (..), MonadLogger (..),
                                              ToLogStr (..), logWithoutLoc)
import           Control.Monad.Reader   as X (MonadReader (..), local,
                                              runReaderT)
import           Control.Monad.Trans    as X (lift)
import           Data.Bifunctor         as X (bimap, first, second)
import           Data.ByteString        as X (ByteString)
import qualified Data.ByteString.Char8  as B8
import           Data.Foldable          as X (asum, fold, for_, sequence_)
import           Data.Function          as X (fix)
import           Data.Functor           as X (void)
import           Data.Maybe             as X (catMaybes, fromJust, isJust,
                                              isNothing, listToMaybe, mapMaybe,
                                              maybeToList)
import           Data.Proxy             as X (Proxy (..))
import           Data.Semigroup         as X (Semigroup (..))
import           Data.String            as X (IsString (..))
import           Data.Text              as X (Text)
import           Data.Traversable       as X (sequence)
import           Data.Word              as X
import           GHC.Generics           as X (Generic)
import           Prelude                as X hiding (sequence, sequence_)
import           Text.Show.Pretty       as X (pPrint, ppShow)


-- | Panic loudly. The impossible has happened, and the only sensible thing to
-- do is crash right now.
data Panic
  = forall a. Show a => Panic !Text !a
  deriving anyclass Exception
deriving instance Show Panic -- Wtf GHC


panic :: (Show a, MonadIO m) => Text -> a -> m void
panic s x =
  liftIO (throwIO (Panic s x))


-- | Convenience function for logging a showable thing with a text tag.
(^.^) :: Text -> a -> (Text, a)
(^.^) = (,)
infixr 0 ^.^


indentPretty :: Int -> ByteString -> ByteString
indentPretty n = B8.unlines . map (indent `B8.append`) . B8.lines
  where indent = B8.replicate n ' '


prettyBS :: Show a => a -> ByteString
prettyBS = B8.pack . ppShow


logPretty :: (MonadLogger m, Show a) => LogLevel -> String -> a -> m ()
logPretty lvl str a = do
  let prettyStr = unlines $ map ("  " ++) $ lines $ ppShow a
  logWithoutLoc "" lvl $ unlines [str, prettyStr]
