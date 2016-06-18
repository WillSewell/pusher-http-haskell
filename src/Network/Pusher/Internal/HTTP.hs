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
module Network.Pusher.Internal.HTTP (get, post) where

import Control.Arrow (second)
import Control.Exception (displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), throwE)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8')
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody(RequestBodyLBS)
  , Response
  , httpLbs
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

import Network.Pusher.Error (PusherError(..))
import Network.Pusher.Internal (PusherRequestParams(PusherRequestParams))
import Network.Pusher.Internal.Util (show')

-- |Issue an HTTP GET request. On a 200 response, the response body is returned.
-- On failure, an error will be thrown into the MonadError instance.
get
  :: A.FromJSON a
  => Manager
  -> PusherRequestParams
  -> ExceptT PusherError IO a
  -- ^The body of the response
get connManager (PusherRequestParams ep qs) = do
  req <- ExceptT $ return $ mkRequest ep qs
  resp <- doReqest connManager req
  either
    (throwE . PusherInvalidResponseError . T.pack)
    return
    (A.eitherDecode resp)

-- |Issue an HTTP POST request.
post
  :: A.ToJSON a
  => Manager
  -> PusherRequestParams
  -> a
  -> ExceptT PusherError IO ()
post connManager (PusherRequestParams ep qs) body = do
  req <- ExceptT $ return $ mkPost (A.encode body) <$> mkRequest ep qs
  _ <- doReqest connManager req
  return ()

mkRequest
  :: T.Text
  -> [(B.ByteString, B.ByteString)]
  -> Either PusherError Request
mkRequest ep qs =
  case parseUrl $ T.unpack ep of
    Nothing -> Left $ PusherArgumentError $ "failed to parse url: " <> ep
    Just req -> Right $ setQueryString (map (second Just) qs) req

mkPost :: BL.ByteString -> Request -> Request
mkPost body req =
  req
    { method = methodPost
    , requestHeaders = [(hContentType, "application/json")]
    , requestBody = RequestBodyLBS body
    }

doReqest :: Manager -> Request -> ExceptT PusherError IO BL.ByteString
doReqest connManager req = do
  response <- liftIO $ httpLbs req connManager
  let status = responseStatus response
  if statusCode status == 200 then
    return $ responseBody response
  else
    let decoded = decodeUtf8' $ statusMessage status in
    throwE $
      either
        (PusherInvalidResponseError . T.pack . displayException)
        PusherNon200ResponseError
        decoded
