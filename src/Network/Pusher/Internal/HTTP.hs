{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Network.Pusher.Internal.HTTP
Description : Functions for issuing HTTP requests
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

A layer on top of the HTTP functions in the Wreq library which lifts the return
values to the typclasses we are using in this library. Non 200 responses are
converted into MonadError errors.
-}
module Network.Pusher.Internal.HTTP
  ( RequestParams(..)
  , RequestQueryString
  , RequestBody
  , get
  , post
  ) where

import Control.Arrow (second)
import Control.Exception (displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), throwE)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified Network.HTTP.Client as HTTP.Client
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Status (statusCode, statusMessage)

import Network.Pusher.Error (PusherError(..))

data RequestParams = RequestParams
  { requestEndpoint :: T.Text
  -- ^The API endpoint, for example http://api.pusherapp.com/apps/123/events
  , requestQueryString :: RequestQueryString
  -- ^List of query string parameters as key-value tuples
  }

type RequestQueryString = [(B.ByteString, B.ByteString)]

type RequestBody = A.Value

-- |Issue an HTTP GET request. On a 200 response, the response body is returned.
-- On failure, an error will be thrown into the MonadError instance.
get
  :: A.FromJSON a
  => HTTP.Client.Manager
  -> RequestParams
  -> ExceptT PusherError IO a -- ^The body of the response
get connManager (RequestParams ep qs) = do
  req <- ExceptT $ return $ mkRequest ep qs
  resp <- doReqest connManager req
  either
    (throwE . PusherInvalidResponseError . T.pack)
    return
    (A.eitherDecode resp)

-- |Issue an HTTP POST request.
post
  :: A.ToJSON a
  => HTTP.Client.Manager -> RequestParams -> a -> ExceptT PusherError IO ()
post connManager (RequestParams ep qs) body = do
  req <- ExceptT $ return $ mkPost (A.encode body) <$> mkRequest ep qs
  _ <- doReqest connManager req
  return ()

mkRequest :: T.Text
          -> RequestQueryString
          -> Either PusherError HTTP.Client.Request
mkRequest ep qs =
  case parseRequest $ T.unpack ep of
    Nothing -> Left $ PusherArgumentError $ "failed to parse url: " <> ep
    Just req -> Right $ HTTP.Client.setQueryString (map (second Just) qs) req
  where

#if MIN_VERSION_http_client(0,4,30)
  parseRequest = HTTP.Client.parseRequest
#else
  parseRequest = HTTP.Client.parseUrl
#endif
mkPost :: BL.ByteString -> HTTP.Client.Request -> HTTP.Client.Request
mkPost body req =
  req
  { HTTP.Client.method = methodPost
  , HTTP.Client.requestHeaders = [(hContentType, "application/json")]
  , HTTP.Client.requestBody = HTTP.Client.RequestBodyLBS body
  }

doReqest
  :: HTTP.Client.Manager
  -> HTTP.Client.Request
  -> ExceptT PusherError IO BL.ByteString
doReqest connManager req = do
  response <- liftIO $ HTTP.Client.httpLbs req connManager
  let status = HTTP.Client.responseStatus response
  if statusCode status == 200
    then return $ HTTP.Client.responseBody response
    else let decoded = decodeUtf8' $ statusMessage status
         in throwE $
            either
              (PusherInvalidResponseError . T.pack . displayException)
              PusherNon200ResponseError
              decoded
