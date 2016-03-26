{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Network.Pusher.Internal.HTTP
Description : Functions for issuing HTTP requests
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

A layer on top of the HTTP functions in the Wreq library which lifts the return
values to the typclasses we are using in this library. Non 200 responses are
converted into MonadError errors.
-}
module Network.Pusher.Internal.HTTP (MonadHTTP(..), get, post) where

import Control.Arrow (second)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8')
import Network.HTTP.Client
  ( Manager
  , RequestBody(RequestBodyLBS)
  , Response
  , method
  , parseUrl
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , setQueryString
  )
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Control.Monad.Pusher.HTTP (MonadHTTP(httpLbs))
import Network.Pusher.Internal.Util (show')

-- |Issue an HTTP GET request. On a 200 response, the response body is returned.
-- On failure, an error will be thrown into the MonadError instance.
get
  :: (A.FromJSON a, Functor m, MonadHTTP m)
  => Manager
  -> B.ByteString
  -- ^The API endpoint, for example http://api.pusherapp.com/apps/123/events
  -> [(B.ByteString, B.ByteString)]
  -- ^List of query string parameters as key-value tuples
  -> ExceptT T.Text m a
  -- ^The body of the response
get connManager ep qs = do
  resp <- makeRequest connManager ep qs Nothing
  when200 resp $
    either
      (throwE . T.pack)
      return
      (A.eitherDecode $ responseBody resp)

-- |Issue an HTTP POST request.
post
  :: (A.ToJSON a, MonadHTTP m)
  => Manager
  -> B.ByteString
  -> [(B.ByteString, B.ByteString)]
  -> a
  -> ExceptT T.Text m ()
post connManager ep qs body = do
  resp <- makeRequest connManager ep qs (Just $ A.encode body)
  errorOn200 resp

-- |Make a request by building up an http-client Request data structure, and
-- performing the IO action.
makeRequest
  :: (Functor m, MonadHTTP m)
  => Manager
  -> B.ByteString
  -> [(B.ByteString, B.ByteString)]
  -> Maybe BL.ByteString
  -> ExceptT T.Text m (Response BL.ByteString)
makeRequest connManager ep qs body =
  case parseUrl $ BC.unpack ep of
    Nothing ->
      throwE $ "failed to parse url: " <> either show' id (decodeUtf8' ep)
    Just req -> do
      let
        req' = setQueryString (map (second Just) qs) req
        req'' = case body of
          Just b -> req'
            { method = methodPost
            , requestHeaders = [(hContentType, "application/json")]
            , requestBody = RequestBodyLBS b
            }
          Nothing -> req'
      lift $ httpLbs req'' connManager

when200 :: (Monad m) => Response BL.ByteString -> ExceptT T.Text m  a -> ExceptT T.Text m a
when200 response run =
  let status = responseStatus response in
  if statusCode status == 200 then
    run
  else
    throwE $ either show' id $ decodeUtf8' $ statusMessage status

errorOn200 :: (Monad m) => Response BL.ByteString -> ExceptT T.Text m ()
errorOn200 response = when200 response (return ())
