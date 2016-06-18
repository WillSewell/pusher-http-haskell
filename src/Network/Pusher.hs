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
  -- * The Pusher config type
    Pusher(..)
  , Credentials(..)
  , getPusher
  , getPusherWithHost
  , getPusherWithConnManager
  -- * Events
  , trigger
  -- * Channel queries
  , channels
  , channel
  , users
  -- * Authentication
  , authenticatePresence
  , authenticatePrivate
  -- * Errors
  , PusherError(..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Network.Pusher.Data
  ( Pusher(..)
  , Credentials(..)
  , getPusher
  , getPusherWithHost
  , getPusherWithConnManager
  )
import Network.Pusher.Error(PusherError(..))
import Network.Pusher.Internal.Auth
  ( authenticatePresence
  , authenticatePrivate
  , makeQS
  )
import Network.Pusher.Internal.Util (getTime)
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
import qualified Network.Pusher.Internal as Pusher
import qualified Network.Pusher.Internal.HTTP as HTTP

-- |Trigger an event to one or more channels.
trigger
  :: MonadIO m
  => Pusher
  -> [Channel]
  -- ^The list of channels to trigger to
  -> T.Text
  -- ^The event
  -> T.Text
  -- ^The data to send (often encoded JSON)
  -> Maybe T.Text
  -- ^An optional socket ID of a connection you wish to exclude
  -> m (Either PusherError ())
trigger pusher chans event dat socketId =
  liftIO $ runExceptT $ do
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
  -> m (Either PusherError ChannelsInfo)
  -- ^The returned data
channels pusher channelTypeFilter prefixFilter attributes =
  liftIO $ runExceptT $ do
    requestParams <-
      liftIO $
        Pusher.mkChannelsRequest
          pusher
          channelTypeFilter
          prefixFilter
          attributes <$>
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
  liftIO $ runExceptT $ do
    requestParams <-
      liftIO $ Pusher.mkChannelRequest pusher chan attributes <$> getTime
    HTTP.get (pusherConnectionManager pusher) requestParams

-- |Get a list of users in a presence channel.
users
  :: MonadIO m
  => Pusher
  -> Channel
  -> m (Either PusherError Users)
users pusher chan =
  liftIO $ runExceptT $ do
    requestParams <- liftIO $ Pusher.mkUsersRequest pusher chan <$> getTime
    HTTP.get (pusherConnectionManager pusher) requestParams
