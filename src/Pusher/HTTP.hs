{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Pusher.HTTP
Description : Functions for issuing HTTP requests
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

A layer on top of the HTTP functions in the Wreq library which lifts the return
values to the typclasses we are using in this library. Non 200 responses are
converted into MonadError errors.
-}
module Pusher.HTTP (MonadHTTP(..), get, post) where

import Data.Text.Encoding (decodeUtf8')
import Control.Applicative ((<$>))
import Control.Lens ((&), (^.), (.~))
import Control.Monad.Error (MonadError, throwError)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT

import Control.Monad.Pusher.HTTP (MonadHTTP(getWith, postWith))

-- |Issue an HTTP GET request. On a 200 response, the response body is returned.
-- On failure, an error will be thrown into the MonadError instance.
get
  :: (A.FromJSON a, Functor m, MonadError String m, MonadHTTP m)
  => T.Text
  -- ^The API endpoint, for example http://api.pusherapp.com/apps/123/events
  -> [(T.Text, B.ByteString)]
  -- ^ List of query string parameters as key-value tuples
  -> m a
  -- ^ The body of the response
get ep qs  = do
  opts <- paramOpts <$> decodeParams qs
  r <- getWith opts (T.unpack ep)
  when200 r $
    either
      throwError
      return
      (A.eitherDecode $ r ^. W.responseBody)

-- |Issue an HTTP POST request.
post
  :: (WT.Postable a, Functor m, MonadError String m, MonadHTTP m)
  => T.Text
  -> [(T.Text, B.ByteString)]
  -> a
  -> m ()
post ep qs body = do
  opts <- paramOpts <$> decodeParams qs
  r <- postWith opts (T.unpack ep) body
  errorOn200 r

-- |Convert the values of key-value tuples to Text from ByteString.
decodeParams
  :: (Functor m, MonadError String m)
  => [(T.Text, B.ByteString)] -> m [(T.Text, T.Text)]
decodeParams qs =
  either
    (throwError . show)
    return
    (mapM (\(k, v) -> (k,) <$> decodeUtf8' v) qs)

-- |Build Wreq request Options from a list of query string parameters.
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
