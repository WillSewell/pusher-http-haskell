{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Pusher.HTTP (get, post) where

import Data.Text.Encoding (decodeUtf8')
import Control.Applicative ((<$>))
import Control.Lens ((&), (^.), (.~))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error (MonadError, throwError)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT

get
  :: (A.FromJSON a, MonadError String m, MonadIO m)
  => T.Text
  -> [(T.Text, B.ByteString)]
  -> m a
get ep qs  = do
  params <- either
    (throwError . show)
    return
    (mapM (\(k, v) -> (k,) <$> decodeUtf8' v) qs)
  let opts = W.defaults & W.params .~ params
  r <- liftIO $ W.getWith opts (T.unpack ep)
  let status = r ^. W.responseStatus
  if status ^. W.statusCode == 200 then do
     liftIO $ print (r ^. W.responseBody)
     either throwError return (A.eitherDecode $ r ^. W.responseBody)
  else
     throwError $ show $ status ^. W.statusMessage

post
  :: (WT.Postable a, MonadError String m, MonadIO m)
  => T.Text
  -> [(T.Text, B.ByteString)]
  -> a
  -> m ()
post ep qs body = do
  params <- either
    (throwError . show)
    return
    (mapM (\(k, v) -> (k,) <$> decodeUtf8' v) qs)
  let opts = W.defaults & W.params .~ params
  r <- liftIO $ W.postWith opts (T.unpack ep) body
  let status = r ^. W.responseStatus
  unless
    (status ^. W.statusCode == 200)
    (throwError $ show $ status ^. W.statusMessage)
