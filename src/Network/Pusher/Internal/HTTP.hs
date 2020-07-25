{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Network.Pusher.Internal.HTTP
-- Description : Functions for issuing HTTP requests
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
--
-- A layer on top of the HTTP functions in the http-client library which lifts the return
-- values to the typclasses we are using in this library. Non 200 responses are
-- converted into MonadError errors.
module Network.Pusher.Internal.HTTP
  ( RequestParams (..),
    get,
    post,
  )
where

import Control.Exception (displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, throwE)
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
  ExceptT PusherError IO a
get connManager (RequestParams host port path query) = do
  let req = mkRequest host port path query
  resp <- doRequest connManager req
  either
    (throwE . PusherInvalidResponseError . T.pack)
    return
    (A.eitherDecode resp)

-- | Issue an HTTP POST request.
post ::
  A.ToJSON a =>
  HTTP.Client.Manager ->
  RequestParams ->
  a ->
  ExceptT PusherError IO ()
post connManager (RequestParams host port path query) body = do
  let req = mkPost (A.encode body) (mkRequest host port path query)
  _ <- doRequest connManager req
  return ()

mkRequest ::
  B.ByteString ->
  Word16 ->
  B.ByteString ->
  Query ->
  HTTP.Client.Request
mkRequest host port path query =
  HTTP.Client.setQueryString query $
    HTTP.Client.defaultRequest
      { HTTP.Client.secure = True,
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
  ExceptT PusherError IO BL.ByteString
doRequest connManager req = do
  response <- liftIO $ HTTP.Client.httpLbs req connManager
  let status = HTTP.Client.responseStatus response
  if statusCode status == 200
    then return $ HTTP.Client.responseBody response
    else
      let decoded = decodeUtf8' $ statusMessage status
       in throwE $
            either
              (PusherInvalidResponseError . T.pack . displayException)
              PusherNon200ResponseError
              decoded
