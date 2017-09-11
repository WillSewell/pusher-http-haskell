{-|
Module      : Network.Pusher.Internal
Description : Pure functions called by the public interface
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental
-}
module Network.Pusher.Internal
  ( mkTriggerRequest
  , mkChannelsRequest
  , mkChannelRequest
  , mkUsersRequest
  , mkNotifyRequest
  ) where

import Control.Monad (when)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Network.Pusher.Data
       (Channel, ChannelType, Credentials(..), Event, EventData,
        Pusher(..), SocketID, Notification(..), renderChannel,
        renderChannelPrefix)
import Network.Pusher.Error (PusherError(..))
import Network.Pusher.Internal.Auth (makeQS)
import Network.Pusher.Internal.HTTP
       (RequestBody, RequestParams(RequestParams), RequestQueryString)
import Network.Pusher.Protocol
       (ChannelInfoQuery, ChannelsInfoQuery, toURLParam)

mkTriggerRequest
  :: Pusher
  -> [Channel]
  -> Event
  -> EventData
  -> Maybe SocketID
  -> Int
  -> Either PusherError (RequestParams, RequestBody)
mkTriggerRequest pusher chans event dat socketId time = do
  when
    (length chans > 10)
    (Left $ PusherArgumentError "Must be less than 10 channels")
  let body =
        A.object $
        [ ("name", A.String event)
        , ("channels", A.toJSON (map (A.String . renderChannel) chans))
        , ("data", A.String dat)
        ] ++
        maybeToList (fmap (\sID -> ("socket_id", A.String sID)) socketId)
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
  -> RequestParams
mkChannelsRequest pusher channelTypeFilter prefixFilter attributes time =
  let prefix = maybe "" renderChannelPrefix channelTypeFilter <> prefixFilter
      params =
        [ ("info", encodeUtf8 $ toURLParam attributes)
        , ("filter_by_prefix", encodeUtf8 prefix)
        ]
  in mkGetRequest pusher "channels" params time

mkChannelRequest :: Pusher
                 -> Channel
                 -> ChannelInfoQuery
                 -> Int
                 -> RequestParams
mkChannelRequest pusher chan attributes time =
  let params = [("info", encodeUtf8 $ toURLParam attributes)]
      subPath = "channels/" <> renderChannel chan
  in mkGetRequest pusher subPath params time

mkUsersRequest :: Pusher -> Channel -> Int -> RequestParams
mkUsersRequest pusher chan time =
  let subPath = "channels/" <> renderChannel chan <> "/users"
  in mkGetRequest pusher subPath [] time

mkNotifyRequest :: Pusher
                -> Notification
                -> Int
                -> Either PusherError (RequestParams, RequestBody)
mkNotifyRequest pusher notification time = do
  let body = A.toJSON notification
      bodyBS = BL.toStrict $ A.encode body
  when (B.length bodyBS > 10000) $
    Left $ PusherArgumentError "Body must be less than 10000KB"
  return $ (mkNotifyPostRequest pusher "notifications" [] bodyBS time, body)

mkGetRequest :: Pusher -> T.Text -> RequestQueryString -> Int -> RequestParams
mkGetRequest pusher subPath params time =
  let (ep, fullPath) = mkEndpoint pusher subPath
      qs = mkQS pusher "GET" fullPath params "" time
  in RequestParams ep qs

mkPostRequest :: Pusher
              -> T.Text
              -> RequestQueryString
              -> B.ByteString
              -> Int
              -> RequestParams
mkPostRequest pusher subPath params bodyBS time =
  let (ep, fullPath) = mkEndpoint pusher subPath
      qs = mkQS pusher "POST" fullPath params bodyBS time
  in RequestParams ep qs

mkNotifyPostRequest :: Pusher
                    -> T.Text
                    -> RequestQueryString
                    -> B.ByteString
                    -> Int
                    -> RequestParams
mkNotifyPostRequest pusher subPath params bodyBS time =
  let (ep, fullPath) = mkNotifyEndpoint pusher subPath
      qs = mkQS pusher "POST" fullPath params bodyBS time
  in RequestParams ep qs

-- |Build a full endpoint from the details in Pusher and the subPath.
mkEndpoint
  :: Pusher
  -> T.Text -- ^The subpath of the specific request, e.g "events/channel-name"
  -> (T.Text, T.Text) -- ^The full endpoint, and just the path component
mkEndpoint pusher subPath =
  let fullPath = pusherPath pusher <> subPath
      endpoint = pusherHost pusher <> fullPath
  in (endpoint, fullPath)

-- |Build a full endpoint for push notifications from the details in Pusher and
-- the subPath
mkNotifyEndpoint
  :: Pusher
  -> T.Text -- ^ The subpath of the specific request
  -> (T.Text, T.Text) -- ^ The full endpoint and just the path component
mkNotifyEndpoint pusher subPath =
  let fullPath = pusherNotifyPath pusher <> subPath
      endpoint = pusherNotifyHost pusher <> fullPath
  in (endpoint, fullPath)

mkQS
  :: Pusher
  -> T.Text
  -> T.Text
  -> RequestQueryString
  -> B.ByteString
  -> Int
  -> RequestQueryString
mkQS pusher =
  let credentials = pusherCredentials pusher
  in makeQS (credentialsAppKey credentials) (credentialsAppSecret credentials)
