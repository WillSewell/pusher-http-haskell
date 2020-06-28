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
-- First create a 'Pusher' data structure with your Pusher Channels 'Credentials',
-- and then call the functions defined in this module to make the HTTP requests.
--
-- If any of the requests fail, the return values of the functions will result in
-- a 'Left' 'PusherError' when run.
--
-- An example of how you would use these functions:
--
-- @
--   let
--     credentials = 'Credentials'
--       { 'credentialsAppID'     = 123
--       , 'credentialsAppKey'    = "wrd12344rcd234"
--       , 'credentialsAppSecret' = "124df34d545v"
--       , 'credentialsCluster'   = Nothing
--       }
--   pusher <- 'getPusher' credentials
--
--   result <-
--     'trigger' pusher ['Channel' 'Public' "my-channel"] "my-event" "my-data" Nothing
--
--   case result of
--     Left e -> putStrLn $ displayException e
--     Right resp -> print resp
--
-- @
--
-- There is a simple working example in the example/ directory.
--
-- See https://pusher.com/docs/channels/server_api/http-api for more detail on the
-- HTTP requests.
module Network.Pusher
  ( -- * Data types

    -- ** Pusher config type
    Pusher (..),
    Credentials (..),
    getPusher,
    getPusherWithHost,
    getPusherWithConnManager,

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
import Control.Monad.Trans.Except
  ( ExceptT (ExceptT),
    runExceptT,
  )
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Network.Pusher.Data
  ( Credentials (..),
    Pusher (..),
    getPusher,
    getPusherWithConnManager,
    getPusherWithHost,
  )
import Network.Pusher.Error (PusherError (..))
import qualified Network.Pusher.Internal as Pusher
import Network.Pusher.Internal.Auth
  ( authenticatePresence,
    authenticatePrivate,
  )
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
trigger pusher chans event dat socketId = liftIO $ runExceptT $ do
  (requestParams, requestBody) <-
    ExceptT $
      Pusher.mkTriggerRequest pusher chans event dat socketId
        <$> getSystemTimeSeconds
  HTTP.post (pusherConnectionManager pusher) requestParams requestBody

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
channels pusher prefixFilter attributes =
  liftIO $ runExceptT $ do
    requestParams <-
      liftIO $
        Pusher.mkChannelsRequest
          pusher
          prefixFilter
          attributes
          <$> getSystemTimeSeconds
    HTTP.get (pusherConnectionManager pusher) requestParams

-- | Query for information on a single channel.
channel ::
  MonadIO m =>
  Pusher ->
  T.Text ->
  -- | Can query user count and also subscription count (if enabled).
  ChannelInfoQuery ->
  m (Either PusherError FullChannelInfo)
channel pusher chan attributes = liftIO $ runExceptT $ do
  requestParams <-
    liftIO $
      Pusher.mkChannelRequest pusher chan attributes
        <$> getSystemTimeSeconds
  HTTP.get (pusherConnectionManager pusher) requestParams

-- | Get a list of users in a presence channel.
users :: MonadIO m => Pusher -> T.Text -> m (Either PusherError Users)
users pusher chan = liftIO $ runExceptT $ do
  requestParams <-
    liftIO $ Pusher.mkUsersRequest pusher chan <$> getSystemTimeSeconds
  HTTP.get (pusherConnectionManager pusher) requestParams

-- | Parse webhooks from a list of HTTP headers and a HTTP body given their
--  'AppKey' matches the one in our Pusher Channels credentials and the webhook
--  is correctly encrypted by the corresponding 'AppSecret'.
parseWebhookPayload ::
  Pusher ->
  [(BC.ByteString, BC.ByteString)] ->
  BC.ByteString ->
  Maybe WebhookPayload
parseWebhookPayload pusher =
  let credentials = pusherCredentials pusher
      ourAppKey = credentialsAppKey credentials
      ourAppSecret = credentialsAppSecret credentials
      lookupKeysSecret whAppKey =
        if whAppKey == ourAppKey then Just ourAppSecret else Nothing
   in parseWebhookPayloadWith lookupKeysSecret
