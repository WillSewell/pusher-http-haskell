{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Pusher.HTTP (get, post) where

import Data.Text.Encoding (decodeUtf8')
import Control.Applicative ((<$>))
import Control.Lens ((&), (^.), (.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error (MonadError, throwError)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT

get
  :: (A.FromJSON a, Functor m, MonadError String m, MonadIO m)
  => T.Text
  -> [(T.Text, B.ByteString)]
  -> m a
get ep qs  = do
  opts <- paramOpts <$> decodeParams qs
  r <- liftIO $ W.getWith opts (T.unpack ep)
  when200 r $
    either
      throwError
      return
      (A.eitherDecode $ r ^. W.responseBody)

post
  :: (WT.Postable a, Functor m, MonadError String m, MonadIO m)
  => T.Text
  -> [(T.Text, B.ByteString)]
  -> a
  -> m ()
post ep qs body = do
  opts <- paramOpts <$> decodeParams qs
  r <- liftIO $ W.postWith opts (T.unpack ep) body
  errorOn200 r

decodeParams
  :: (Functor m, MonadError String m)
  => [(T.Text, B.ByteString)] -> m [(T.Text, T.Text)]
decodeParams qs =
  either
    (throwError . show)
    return
    (mapM (\(k, v) -> (k,) <$> decodeUtf8' v) qs)

paramOpts :: [(T.Text, T.Text)] -> W.Options
paramOpts params = W.defaults & W.params .~ params

when200 :: (MonadError String m) => W.Response BL.ByteString -> m a -> m a
when200 response run =
  let status = response ^. W.responseStatus in
  if status ^. W.statusCode == 200 then
     run
  else
     throwError $ show $ status ^. W.statusMessage

errorOn200 :: (MonadError String m) => W.Response BL.ByteString -> m ()
errorOn200 response = when200 response (return ())
