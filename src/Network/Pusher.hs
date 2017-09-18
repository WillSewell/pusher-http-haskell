{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Network.Pusher
Description : Haskell interface to the Pusher HTTP API
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

Exposes the functions necessary for interacting with the Pusher HTTP API, as
well as functions for generating auth signatures for private and presence
channels.

First create a 'Pusher' data structure with your Pusher 'Credentials', and then
call the functions defined in this module to make the HTTP requests.

If any of the requests fail, the return values of the functions will result in
a 'Left' 'PusherError' when run.

An example of how you would use these functions:

@
  let
    credentials = Credentials
      { credentialsAppID     = 123
      , credentialsAppKey    = wrd12344rcd234
      , credentialsAppSecret = 124df34d545v
      , credentialsCluster   = Nothing
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
module Network.Pusher
  -- * Data types
  -- ** Pusher config type
  ( Pusher(..)
  , Credentials(..)
  , Cluster(..)
  , AppID
  , AppKey
  , AppSecret
  , getPusher
  , getPusherWithHost
  , getPusherWithConnManager
  -- ** Channels
  , Channel(..)
  , ChannelName
  , ChannelType(..)
  , renderChannel
  , renderChannelPrefix
  , parseChannel
  -- ** Events
  , Event
  , EventData
  , SocketID
  -- * HTTP Requests
  -- ** Trigger events
  , trigger
  -- ** Channel queries
  , channels
  , channel
  , users
  -- * Authentication
  , AuthString
  , AuthSignature
  , authenticatePresence
  , authenticatePrivate
  -- * Errors
  , PusherError(..)
  -- * Webhooks
  , parseWebhookPayload
  , WebhookEv(..)
  , WebhookPayload(..)
  , Webhooks(..)
  , parseAppKeyHdr
  , parseAuthSignatureHdr
  , parseWebhooksBody
  , verifyWebhooksBody
  , parseWebhookPayloadWith
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import qualified Data.Text as T

import Network.Pusher.Data
       (AppID, AppKey, AppSecret, Channel(..), ChannelName,
        ChannelType(..), Credentials(..), Cluster(..), Event, EventData,
        Pusher(..), SocketID, getPusher, getPusherWithHost,
        getPusherWithConnManager, parseChannel, renderChannel,
        renderChannelPrefix)
import Network.Pusher.Error (PusherError(..))
import qualified Network.Pusher.Internal as Pusher
import Network.Pusher.Internal.Auth
       (AuthSignature, AuthString, authenticatePresence,
        authenticatePrivate)
import qualified Network.Pusher.Internal.HTTP as HTTP
import Network.Pusher.Internal.Util (getTime)
import Network.Pusher.Protocol
       (ChannelInfoQuery, ChannelsInfo, ChannelsInfoQuery,
        FullChannelInfo, Users)
import Network.Pusher.Webhook
       (Webhooks(..), WebhookEv(..), WebhookPayload(..),parseAppKeyHdr,parseAuthSignatureHdr,parseWebhooksBody,verifyWebhooksBody,parseWebhookPayloadWith)
import qualified Data.ByteString.Char8 as B

-- |Trigger an event to one or more channels.
trigger
  :: MonadIO m
  => Pusher
  -> [Channel]
  -- ^The list of channels to trigger to
  -> Event
  -> EventData
  -- ^Often encoded JSON
  -> Maybe SocketID
  -- ^An optional socket ID of a connection you wish to exclude
  -> m (Either PusherError ())
trigger pusher chans event dat socketId =
  liftIO $
  runExceptT $ do
    (requestParams, requestBody) <-
      ExceptT $
      Pusher.mkTriggerRequest pusher chans event dat socketId <$> getTime
    HTTP.post (pusherConnectionManager pusher) requestParams requestBody

-- |Query a list of channels for information.
channels
  :: MonadIO m
  => Pusher
  -> Maybe ChannelType
  -- ^Filter by the type of channel
  -> T.Text
  -- ^A channel prefix you wish to filter on
  -> ChannelsInfoQuery
  -- ^Data you wish to query for, currently just the user count
  -> m (Either PusherError ChannelsInfo) -- ^The returned data
channels pusher channelTypeFilter prefixFilter attributes =
  liftIO $
  runExceptT $ do
    requestParams <-
      liftIO $
      Pusher.mkChannelsRequest pusher channelTypeFilter prefixFilter attributes <$>
      getTime
    HTTP.get (pusherConnectionManager pusher) requestParams

-- |Query for information on a single channel.
channel
  :: MonadIO m
  => Pusher
  -> Channel
  -> ChannelInfoQuery
  -- ^Can query user count and also subscription count (if enabled)
  -> m (Either PusherError FullChannelInfo)
channel pusher chan attributes =
  liftIO $
  runExceptT $ do
    requestParams <-
      liftIO $ Pusher.mkChannelRequest pusher chan attributes <$> getTime
    HTTP.get (pusherConnectionManager pusher) requestParams

-- |Get a list of users in a presence channel.
users
  :: MonadIO m
  => Pusher -> Channel -> m (Either PusherError Users)
users pusher chan =
  liftIO $
  runExceptT $ do
    requestParams <- liftIO $ Pusher.mkUsersRequest pusher chan <$> getTime
    HTTP.get (pusherConnectionManager pusher) requestParams

-- Parse webhooks from a list of HTTP headers and a HTTP body given their AppKey
-- matches the one in our Pusher credentials and the webhook is correctly
-- encrypted by the corresponding AppSecret.
parseWebhookPayload
  :: Pusher
  -> [(B.ByteString,B.ByteString)]
  -> B.ByteString
  -> Maybe WebhookPayload
parseWebhookPayload pusher =
  let credentials = pusherCredentials pusher
      ourAppKey = credentialsAppKey credentials
      ourAppSecret = credentialsAppSecret credentials
      lookupKeysSecret whAppKey = if whAppKey == ourAppKey then Just ourAppSecret else Nothing
    in parseWebhookPayloadWith lookupKeysSecret

