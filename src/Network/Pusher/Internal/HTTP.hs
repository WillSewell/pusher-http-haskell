{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Network.Pusher.Internal.HTTP
-- Description : Functions for issuing HTTP requests
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
--
-- A layer on top of the HTTP functions in the http-client library which lifts
-- the return values to the typeclasses we are using in this library. Non 200
-- responses are converted into MonadError errors.
module Network.Pusher.Internal.HTTP
  ( RequestParams (..),
    get,
    post,
  )
where

import Control.Exception (displayException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word16)
import qualified Network.HTTP.Client as HTTP.Client
import Network.HTTP.Types (Query)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Network.Pusher.Error (PusherError (..))

data RequestParams
  = RequestParams
      { -- | The API endpoint, for example http://api.pusherapp.com/apps/123/events.
        requestSecure :: Bool,
        requestHost :: B.ByteString,
        requestPort :: Word16,
        requestPath :: B.ByteString,
        -- | List of query string parameters as key-value tuples.
        requestQueryString :: Query
      }
  deriving (Show)

-- | Issue an HTTP GET request. On a 200 response, the response body is returned.
--  On failure, an error will be thrown into the MonadError instance.
get ::
  A.FromJSON a =>
  HTTP.Client.Manager ->
  RequestParams ->
  IO (Either PusherError a)
get connManager (RequestParams secure host port path query) = do
  let req = mkRequest secure host port path query
  eitherBody <- doRequest connManager req
  return $ case eitherBody of
    Left e -> Left e
    Right body ->
      either
        (Left . PusherInvalidResponseError . T.pack)
        Right
        (A.eitherDecode body)

-- | Issue an HTTP POST request.
post ::
  A.ToJSON a =>
  HTTP.Client.Manager ->
  RequestParams ->
  a ->
  IO (Either PusherError ())
post connManager (RequestParams secure host port path query) body = do
  let req = mkPost (A.encode body) (mkRequest secure host port path query)
  eitherBody <- doRequest connManager req
  return $ either Left (const $ Right ()) eitherBody

mkRequest ::
  Bool ->
  B.ByteString ->
  Word16 ->
  B.ByteString ->
  Query ->
  HTTP.Client.Request
mkRequest secure host port path query =
  HTTP.Client.setQueryString query $
    HTTP.Client.defaultRequest
      { HTTP.Client.secure = secure,
        HTTP.Client.host = host,
        HTTP.Client.port = fromIntegral port,
        HTTP.Client.path = path
      }

mkPost :: BL.ByteString -> HTTP.Client.Request -> HTTP.Client.Request
mkPost body req =
  req
    { HTTP.Client.method = methodPost,
      HTTP.Client.requestHeaders = [(hContentType, "application/json")],
      HTTP.Client.requestBody = HTTP.Client.RequestBodyLBS body
    }

doRequest ::
  HTTP.Client.Manager ->
  HTTP.Client.Request ->
  IO (Either PusherError BL.ByteString)
doRequest connManager req = do
  response <- liftIO $ HTTP.Client.httpLbs req connManager
  let status = HTTP.Client.responseStatus response
  return $
    if statusCode status == 200
      then Right $ HTTP.Client.responseBody response
      else
        let decoded = decodeUtf8' $ statusMessage status
         in Left $
              either
                (PusherInvalidResponseError . T.pack . displayException)
                PusherNon200ResponseError
                decoded
