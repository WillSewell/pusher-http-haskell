{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Network.Pusher
-- Description : Haskell interface to the Pusher Channels HTTP API
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
--
-- Exposes the functions necessary for interacting with the Pusher Channels HTTP
-- API, as well as functions for generating auth signatures for private and
-- presence channels.
--
-- First create a 'Settings'. The easiest way of doing this is by using
-- 'defaultSettings'. From that you can use 'newPusher' to create a 'Pusher'
-- instance and then call the functions defined in this module to make the HTTP
-- requests.
--
-- If any of the requests fail, the return values of the functions will result in
-- a 'Left' 'PusherError' when run.
--
-- An example of how you would use these functions:
--
-- @
--   let
--     settings =
--       'defaultSettings'
--         { 'pusherAddress' = 'Cluster' "mt1",
--           'pusherAppID' = 123,
--           'pusherToken' = 'Token' "wrd12344rcd234" "124df34d545v"
--         }
--   pusher <- 'newPusher' settings
--
--   result <-
--     'trigger' pusher ["my-channel"] "my-event" "my-data" Nothing
--
--   case result of
--     Left e -> putStrLn $ displayException e
--     Right resp -> print resp
--
-- @
--
-- There are simple working examples in the example/ directory.
--
-- See https://pusher.com/docs/channels/server_api/http-api for more detail on the
-- HTTP requests.
module Network.Pusher
  ( -- * Data types

    -- ** Settings
    Settings (..),
    defaultSettings,
    Token (..),
    Address (..),

    -- ** Main Pusher type
    Pusher,
    newPusher,
    newPusherWithConnManager,

    -- * HTTP Requests

    -- ** Trigger events
    trigger,

    -- ** Channel queries
    channels,
    channel,
    users,

    -- * Authentication
    authenticatePresence,
    authenticatePrivate,

    -- * Errors
    PusherError (..),

    -- * Webhooks
    parseWebhookPayload,
    WebhookEv (..),
    WebhookPayload (..),
    Webhooks (..),
    parseAppKeyHdr,
    parseAuthSignatureHdr,
    parseWebhooksBody,
    verifyWebhooksBody,
    parseWebhookPayloadWith,
  )
where

import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.Pusher.Data
  ( Address (..),
    Pusher (..),
    Settings (..),
    Token (..),
    defaultSettings,
    newPusher,
    newPusherWithConnManager,
  )
import Network.Pusher.Error (PusherError (..))
import qualified Network.Pusher.Internal as Pusher
import qualified Network.Pusher.Internal.Auth as Auth
import qualified Network.Pusher.Internal.HTTP as HTTP
import Network.Pusher.Internal.Util (getSystemTimeSeconds)
import Network.Pusher.Protocol
  ( ChannelInfoQuery,
    ChannelsInfo,
    ChannelsInfoQuery,
    FullChannelInfo,
    Users,
  )
import Network.Pusher.Webhook
  ( WebhookEv (..),
    WebhookPayload (..),
    Webhooks (..),
    parseAppKeyHdr,
    parseAuthSignatureHdr,
    parseWebhookPayloadWith,
    parseWebhooksBody,
    verifyWebhooksBody,
  )

-- | Trigger an event to one or more channels.
trigger ::
  MonadIO m =>
  Pusher ->
  -- | The list of channels to trigger to.
  [T.Text] ->
  -- | Event name.
  T.Text ->
  -- | Event data. Often encoded JSON.
  T.Text ->
  -- | An optional socket ID of a connection you wish to exclude.
  Maybe T.Text ->
  m (Either PusherError ())
trigger pusher chans event dat socketId = do
  (requestParams, requestBody) <-
    Pusher.mkTriggerRequest pusher chans event dat socketId
      <$> getSystemTimeSeconds
  liftIO $ HTTP.post (pConnectionManager pusher) requestParams requestBody

-- | Query a list of channels for information.
channels ::
  MonadIO m =>
  Pusher ->
  -- | A channel prefix you wish to filter on.
  T.Text ->
  -- | Data you wish to query for, currently just the user count.
  ChannelsInfoQuery ->
  -- | The returned data.
  m (Either PusherError ChannelsInfo)
channels pusher prefixFilter attributes = do
  requestParams <-
    Pusher.mkChannelsRequest pusher prefixFilter attributes
      <$> getSystemTimeSeconds
  liftIO $ HTTP.get (pConnectionManager pusher) requestParams

-- | Query for information on a single channel.
channel ::
  MonadIO m =>
  Pusher ->
  B.ByteString ->
  -- | Can query user count and also subscription count (if enabled).
  ChannelInfoQuery ->
  m (Either PusherError FullChannelInfo)
channel pusher chan attributes = do
  requestParams <-
    Pusher.mkChannelRequest pusher chan attributes <$> getSystemTimeSeconds
  liftIO $ HTTP.get (pConnectionManager pusher) requestParams

-- | Get a list of users in a presence channel.
users :: MonadIO m => Pusher -> B.ByteString -> m (Either PusherError Users)
users pusher chan = do
  requestParams <- Pusher.mkUsersRequest pusher chan <$> getSystemTimeSeconds
  liftIO $ HTTP.get (pConnectionManager pusher) requestParams

-- | Generate an auth signature of the form "app_key:auth_sig" for a user of a
--  private channel.
authenticatePrivate :: Pusher -> T.Text -> T.Text -> B.ByteString
authenticatePrivate pusher = Auth.authenticatePrivate (pToken pusher)

-- | Generate an auth signature of the form "app_key:auth_sig" for a user of a
--  presence channel.
authenticatePresence ::
  A.ToJSON a => Pusher -> T.Text -> T.Text -> a -> B.ByteString
authenticatePresence pusher = Auth.authenticatePresence (pToken pusher)

-- | Parse webhooks from a list of HTTP headers and a HTTP body given their
--  app key matches the one in our Pusher Channels credentials and the webhook
--  is correctly encrypted by the corresponding app secret.
parseWebhookPayload ::
  Pusher ->
  [(B.ByteString, B.ByteString)] ->
  B.ByteString ->
  Maybe WebhookPayload
parseWebhookPayload pusher =
  let token = pToken pusher
      ourAppKey = tokenKey token
      ourAppSecret = tokenSecret token
      lookupKeysSecret whAppKey =
        if whAppKey == ourAppKey then Just ourAppSecret else Nothing
   in parseWebhookPayloadWith lookupKeysSecret
