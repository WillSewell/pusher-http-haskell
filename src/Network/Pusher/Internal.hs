{-|
Module      : Network.Pusher.Internal
Description : Pure functions called by the public interface
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental
-}
module Network.Pusher.Internal
  ( PusherRequestParams(..)
  , PusherRequestBody
  , mkTriggerRequest
  , mkChannelsRequest
  , mkChannelRequest
  , mkUsersRequest
  ) where

import Control.Monad (when)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Network.Pusher.Data (Pusher(..), Credentials(..))
import Network.Pusher.Error(PusherError(..))
import Network.Pusher.Internal.Auth (makeQS)
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

data PusherRequestParams = PusherRequestParams
  { pusherRequestEndpoint :: T.Text
  -- ^The API endpoint, for example http://api.pusherapp.com/apps/123/events
  , pusherRequestQueryString :: [(B.ByteString, B.ByteString)]
  -- ^List of query string parameters as key-value tuples
  }

type PusherRequestBody = A.Value

mkTriggerRequest
  :: Pusher
  -> [Channel]
  -> T.Text
  -> T.Text
  -> Maybe T.Text
  -> Int
  -> Either PusherError (PusherRequestParams, PusherRequestBody)
mkTriggerRequest pusher chans event dat socketId time = do
    when
      (length chans > 10)
      (Left $ PusherArgumentError "Must be less than 10 channels")
    let
      body = A.object $
        [ ("name", A.String event)
        , ("channels", A.toJSON (map (A.String . renderChannel) chans))
        , ("data", A.String dat)
        ] ++ maybeToList (fmap (\sID ->  ("socket_id", A.String sID)) socketId)
      bodyBS = BL.toStrict $ A.encode body
    when
      (B.length bodyBS > 10000)
      (Left $ PusherArgumentError "Body must be less than 10000KB")
    return (mkPostRequest pusher "events" [] bodyBS time, body)

mkChannelsRequest
  :: Pusher
  -> Maybe ChannelType
  -> T.Text
  -> ChannelsInfoQuery
  -> Int
  -> PusherRequestParams
mkChannelsRequest pusher channelTypeFilter prefixFilter attributes time =
  let
    prefix = maybe "" renderChannelPrefix channelTypeFilter <> prefixFilter
    params =
      [ ("info", encodeUtf8 $ toURLParam attributes)
      , ("filter_by_prefix", encodeUtf8 prefix)
      ]
  in
    mkGetRequest pusher "channels" params time

mkChannelRequest
  :: Pusher
  -> Channel
  -> ChannelInfoQuery
  -> Int
  -> PusherRequestParams
mkChannelRequest pusher chan attributes time =
  let
    params = [("info", encodeUtf8 $ toURLParam attributes)]
    subPath = "channels/" <> renderChannel chan
  in
    mkGetRequest pusher subPath params time

mkUsersRequest :: Pusher -> Channel -> Int -> PusherRequestParams
mkUsersRequest pusher chan time =
  let
    subPath = "channels/" <> renderChannel chan <> "/users"
  in
    mkGetRequest pusher subPath [] time

mkGetRequest
  :: Pusher
  -> T.Text
  -> [(B.ByteString, B.ByteString)]
  -> Int
  -> PusherRequestParams
mkGetRequest pusher subPath params time =
  let
    (fullPath, ep) = mkEndpoint pusher subPath
    qs = mkQS pusher "GET" fullPath params "" time
  in
    PusherRequestParams ep qs

mkPostRequest
  :: Pusher
  -> T.Text
  -> [(B.ByteString, B.ByteString)]
  -> B.ByteString
  -> Int
  -> PusherRequestParams
mkPostRequest pusher subPath params bodyBS time =
  let
    (fullPath, ep) = mkEndpoint pusher subPath
    qs = mkQS pusher "POST" fullPath params bodyBS time
  in
    PusherRequestParams ep qs

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
