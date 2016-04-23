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
    credentials = Credentials
      { credentialsAppID = 123
      , credentialsAppKey = wrd12344rcd234
      , credentialsAppSecret = 124df34d545v
      }
  pusher <- getPusher credentials
  result <-
    trigger pusher [Channel Public "my-channel"] "my-event" "my-data" Nothing
  case result of
    Left e -> error e
    Right resp -> print resp
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
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Control.Monad.Pusher.HTTP (MonadHTTP)
import Control.Monad.Pusher.Time (MonadTime, getPOSIXTime)
import Data.Pusher (Credentials(..), Pusher(..))
import Network.Pusher.Internal.Auth
  ( authenticatePresence
  , authenticatePrivate
  , makeQS
  )
import Network.Pusher.Protocol
  ( Channel
  , ChannelInfoQuery
  , ChannelsInfo
  , ChannelsInfoQuery
  , ChannelType
  , FullChannelInfo
  , Users
  , renderChannel
  , renderChannelPrefix
  , toURLParam
  )
import qualified Network.Pusher.Internal.HTTP as HTTP

-- |Trigger an event to one or more channels.
trigger
  :: (MonadHTTP m, MonadIO m, MonadTime m)
  => Pusher
  -> [Channel] -- ^The list of channels to trigger to
  -> T.Text -- ^The event
  -> T.Text -- ^The data to send (often encoded JSON)
  -> Maybe T.Text -- ^An optional socket ID of a connection you wish to exclude
  -> m (Either T.Text ())
trigger pusher chans event dat socketId =
  runExceptT $ do
    when
      (length chans > 10)
      (throwE "Must be less than 10 channels")

    let
      body = A.object $
        [ ("name", A.String event)
        , ("channels", A.toJSON (map (A.String . renderChannel) chans))
        , ("data", A.String dat)
        ] ++ maybeToList (fmap (\sID ->  ("socket_id", A.String sID)) socketId)
      bodyBS = BL.toStrict $ A.encode body
    when
      (B.length bodyBS > 10000)
      (throwE "Body must be less than 10000KB")

    post pusher "events" [] body bodyBS

-- |Query a list of channels for information.
channels
  :: (MonadHTTP m, MonadIO m, MonadTime m)
  => Pusher
  -> Maybe ChannelType -- ^Filter by the type of channel
  -> T.Text -- ^A channel prefix you wish to filter on
  -> ChannelsInfoQuery -- ^Data you wish to query for, currently just the user count
  -> m (Either T.Text ChannelsInfo) -- ^The returned data
channels pusher channelTypeFilter prefixFilter attributes =
  let
    prefix = maybe "" renderChannelPrefix channelTypeFilter <> prefixFilter
    params =
      [ ("info", encodeUtf8 $ toURLParam attributes)
      , ("filter_by_prefix", encodeUtf8 prefix)
      ]
  in
    runExceptT $ get pusher "channels" params

-- |Query for information on a single channel.
channel
  :: (MonadHTTP m, MonadIO m, MonadTime m)
  => Pusher
  -> Channel
  -> ChannelInfoQuery  -- ^Can query user count and also subscription count (if enabled)
  -> m (Either T.Text FullChannelInfo)
channel pusher chan attributes =
  let
    params = [("info", encodeUtf8 $ toURLParam attributes)]
    subPath = "channels/" <> renderChannel chan
  in
    runExceptT $ get pusher subPath params

-- |Get a list of users in a presence channel.
users
  :: (MonadHTTP m, MonadIO m, MonadTime m)
  => Pusher
  -> Channel
  -> m (Either T.Text Users)
users pusher chan =
  let
    subPath = "channels/" <> renderChannel chan <> "/users"
  in
    runExceptT $ get pusher subPath []

get
  :: (A.FromJSON a, MonadHTTP m, MonadIO m, MonadTime m)
  => Pusher
  -> T.Text
  -> [(B.ByteString, B.ByteString)]
  -> ExceptT T.Text m a
get pusher subPath params = do
  let (fullPath, ep) = mkEndpoint pusher subPath
  qs <- mkQS pusher "GET" fullPath params "" <$> getPOSIXTime
  HTTP.get (pusherConnectionManager pusher) (encodeUtf8 ep) qs

post
  :: (A.ToJSON a, MonadHTTP m, MonadIO m, MonadTime m)
  => Pusher
  -> T.Text
  -> [(B.ByteString, B.ByteString)]
  -> a
  -> B.ByteString
  -> ExceptT T.Text m ()
post pusher subPath params body bodyBS = do
  let (fullPath, ep) = mkEndpoint pusher subPath
  qs <- mkQS pusher "POST" fullPath params bodyBS <$> getPOSIXTime
  HTTP.post (pusherConnectionManager pusher) (encodeUtf8 ep) qs body

-- |Build a full endpoint from the details in Pusher and the subPath.
mkEndpoint
  :: Pusher
  -> T.Text -- ^The subpath of the specific request, e.g "events/channel-name"
  -> (T.Text, T.Text) -- ^The full endpoint, and just the path component
mkEndpoint pusher subPath =
  let
    fullPath = pusherPath pusher <> subPath
    endpoint = pusherHost pusher <> fullPath
  in
    (endpoint, fullPath)

mkQS
  :: Pusher
  -> T.Text
  -> T.Text
  -> [(B.ByteString, B.ByteString)]
  -> B.ByteString
  -> Int
  -> [(B.ByteString, B.ByteString)]
mkQS pusher =
  let
    credentials = pusherCredentials pusher
  in
    makeQS
      (credentialsAppKey credentials)
      (credentialsAppSecret credentials)
