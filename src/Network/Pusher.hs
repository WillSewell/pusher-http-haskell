{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Network.Pusher
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
module Network.Pusher (
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

import Control.Applicative ((<$>))
import Control.Monad (when)
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
import Control.Monad.Pusher.Time (MonadTime, getPOSIXTime)
import Data.Pusher (Credentials(..), Pusher(..))
import Network.Pusher.Internal.Auth
  ( authenticatePresence
  , authenticatePrivate
  , makeQS
  )
import Network.Pusher.Internal.HTTP (get, post)
import Network.Pusher.Internal.Util (show')
import Network.Pusher.Protocol
  ( Channel
  , ChannelInfo
  , ChannelInfoQuery
  , ChannelsInfo
  , ChannelsInfoQuery
  , ChannelType
  , FullChannelInfo
  , Users
  , toURLParam
  )

-- |Trigger an event to one or more channels.
trigger
  :: MonadPusher m
  => [Channel] -- ^The list of channels to trigger to
  -> T.Text -- ^The event
  -> T.Text -- ^The data to send (often encoded JSON)
  -> Maybe T.Text -- ^An optional socket ID of a connection you wish to exclude
  -> m ()
trigger chans event dat socketId = do
  when
    (length chans > 10)
    (throwError "Must be less than 10 channels")

  let
    body = A.object $
      [ ("name", A.String event)
      , ("channels", A.toJSON (map (A.String . show') chans))
      , ("data", A.String dat)
      ] ++ maybeToList (fmap (\sID ->  ("socket_id", A.String sID)) socketId)
    bodyBS = BL.toStrict $ A.encode body
  when
    (B.length bodyBS > 10000)
    (throwError "Body must be less than 10000KB")

  (ep, path) <- getEndpoint "events"
  qs <- makeQSWithTS "POST" path [] bodyBS
  connManager <- asks pusherConnectionManager
  post connManager (encodeUtf8 ep) qs body

-- |Query a list of channels for information.
channels
  :: MonadPusher m
  => Maybe ChannelType -- ^Filter by the type of channel
  -> T.Text -- ^A channel prefix you wish to filter on
  -> ChannelsInfoQuery -- ^Data you wish to query for, currently just the user count
  -> m ChannelsInfo -- ^The returned data
channels channelTypeFilter prefixFilter attributes = do
  let
    prefix = maybe "" show' channelTypeFilter <> prefixFilter
    params =
      [ ("info", encodeUtf8 $ toURLParam attributes)
      , ("filter_by_prefix", encodeUtf8 prefix)
      ]
  (ep, path) <- getEndpoint "channels"
  qs <- makeQSWithTS "GET" path params ""
  connManager <- asks pusherConnectionManager
  get connManager (encodeUtf8 ep) qs

-- |Query for information on a single channel.
channel
  :: MonadPusher m
  => Channel
  -> ChannelInfoQuery  -- ^Can query user count and also subscription count (if enabled)
  -> m FullChannelInfo
channel chan attributes = do
  let params = [("info", encodeUtf8 $ toURLParam attributes)]
  (ep, path) <- getEndpoint $ "channels/" <> show' chan
  qs <- makeQSWithTS "GET" path params ""
  connManager <- asks pusherConnectionManager
  get connManager (encodeUtf8 ep) qs

-- |Get a list of users in a presence channel.
users
 :: MonadPusher m
 => Channel
 -> m Users
users chan = do
  (ep, path) <- getEndpoint $ "channels/" <> show' chan <> "/users"
  qs <- makeQSWithTS "GET" path [] ""
  connManager <- asks pusherConnectionManager
  get connManager (encodeUtf8 ep) qs

-- |Build a full endpoint from the details in Pusher and the subPath.
getEndpoint
  :: (MonadReader Pusher m)
  => T.Text -- ^The subpath of the specific request, e.g "events/channel-name"
  -> m (T.Text, T.Text) -- ^The full endpoint, and just the path component
getEndpoint subPath = do
  host <- asks pusherHost
  path <- asks pusherPath
  let
    fullPath = path <> subPath
    endpoint = host <> fullPath
  return (endpoint, fullPath)

-- |Impure wrapper around makeQS which gets the current time implicitly.
makeQSWithTS
  :: (Functor m, MonadTime m, MonadReader Pusher m)
  => T.Text
  -> T.Text
  -> [(B.ByteString, B.ByteString)]
  -> B.ByteString
  -> m [(B.ByteString, B.ByteString)]
makeQSWithTS method path params body = do
  appKey <- asks $ credentialsAppKey . pusherCredentials
  appSecret <- asks $ credentialsAppSecret . pusherCredentials
  makeQS appKey appSecret method path params body <$> getPOSIXTime
