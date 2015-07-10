{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Pusher
Description : Haskell interface to the Pusher HTTP API
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

Exposes the functions necessary for interacting with the Pusher HTTP API, as
well as functions for generating auth signatures for private and presence
channels.

The idea is that you must create a Pusher data structure with your credentials,
then your write your block of code that calls functions from this library, and
finally "run" this code with the Pusher data structure you created.

The types of the functions are general enough to allow you to use the provided
PusherM as the return type, or PusherT if you wish to use other monads in your
applications monad transformer stack.

The functions return a MonadError type which means any can fail, and you must
handle these failures in your client application. In the simplist case you can
instantiate the type to an Either and case split on it.

An example of how you would use these functions:

@
  let
    pusher = getPusher $ Credentials
      { credentials'appID = 123
      , credentials'appKey = wrd12344rcd234
      , credentials'appSecret = 124df34d545v
      }
  result <- runPusherT (Pusher.trigger ["my-channel"] "my-event" "my-data") pusher
  case result of
    Right resp -> print resp
    Left e -> error e
@

There is a simple working example in the example/ directory.

See https://pusher.com/docs/rest_api for more detail on the HTTP requests.
-}
module Pusher (
  -- * Events
    trigger
  -- * Channel queries
  , channels
  , channel
  , users
  -- * Authentication
  , authenticatePresence
  , authenticatePrivate
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (MonadReader, asks)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Control.Monad.Pusher (MonadPusher)
import Data.Pusher (Pusher(..))
import Pusher.Auth (authenticatePresence, authenticatePrivate, makeQS)
import Pusher.HTTP (MonadHTTP, get, post)
import Pusher.Protocol
  ( ChannelInfo
  , ChannelInfoQuery
  , ChannelsInfo
  , ChannelsInfoQuery
  , Users
  , toURLParam
  )
import Pusher.Util (getIntPOSIXTime)

-- |Trigger an event to one or more channels.
trigger
  :: MonadPusher m
  => [T.Text] -- ^The list of channels to trigger to
  -> T.Text -- ^The event
  -> T.Text -- ^The data to send (often encoded JSON)
  -> Maybe T.Text -- ^An optional socket ID of a connection you wish to exclude
  -> m ()
trigger channelNames event dat socketId = do
  when
    (length channelNames > 10)
    (throwError "Must be less than 10 channels")

  let
    body = A.object $
      [ ("name", A.String event)
      , ("channels", A.toJSON (map A.String channelNames))
      , ("data", A.String dat)
      ] ++ maybeToList (fmap (\sID ->  ("socket_id", A.String sID)) socketId)
    bodyBS = BL.toStrict $ A.encode body
  when
    (B.length bodyBS > 10000)
    (throwError "Body must be less than 10000KB")

  (ep, path) <- getEndpoint "events"
  qs <- makeQSWithTS "POST" path [] bodyBS
  post ep qs body

-- |Query a list of channels for information.
channels
  :: MonadPusher m
  => T.Text -- ^A channel prefix you wish to filter on
  -> ChannelsInfoQuery -- ^Data you wish to query for, currently just the user count
  -> m ChannelsInfo -- ^The returned data
channels prefix attributes = do
  let
    params =
      [ ("info", encodeUtf8 $ toURLParam attributes)
      , ("filter_by_prefix", encodeUtf8 prefix)
      ]
  (ep, path) <- getEndpoint "channels"
  qs <- makeQSWithTS "GET" path params ""
  get ep qs

-- |Query for information on a single channel.
channel
  :: MonadPusher m
  => T.Text
  -> ChannelInfoQuery  -- ^Can query user count and also subscription count (if enabled)
  -> m ChannelInfo
channel channelName attributes = do
  let params = [("info", encodeUtf8 $ toURLParam attributes)]
  (ep, path) <- getEndpoint $ "channels/" <> channelName
  qs <- makeQSWithTS "GET" path params ""
  get ep qs

-- |Get a list of users in a presence channel.
users
 :: MonadPusher m
 => T.Text -> m Users
users channelName = do
  (ep, path) <- getEndpoint $ "channels/" <> channelName <> "/users"
  qs <- makeQSWithTS "GET" path [] ""
  get ep qs

-- |Build a full endpoint from the details in Pusher and the subPath.
getEndpoint
  :: (MonadReader Pusher m)
  => T.Text -- ^The subpath of the specific request, e.g "events/channel-name"
  -> m (T.Text, T.Text) -- ^The full endpoint, and just the path component
getEndpoint subPath = do
  host <- asks pusher'host
  path <- asks pusher'path
  let
    fullPath = path <> subPath
    endpoint = host <> fullPath
  return (endpoint, fullPath)

-- |Impure wrapper around makeQS which gets the current time implicitly.
makeQSWithTS
  :: (MonadReader Pusher m, MonadIO m, MonadHTTP m)
  => T.Text
  -> T.Text
  -> [(T.Text, B.ByteString)]
  -> B.ByteString
  -> m [(T.Text, B.ByteString)]
makeQSWithTS method path params body =
  makeQS method path params body =<< getIntPOSIXTime
