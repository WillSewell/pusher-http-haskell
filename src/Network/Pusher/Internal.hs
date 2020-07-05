-- |
-- Module      : Network.Pusher.Internal
-- Description : Pure functions called by the public interface
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : experimental
module Network.Pusher.Internal
  ( mkTriggerRequest,
    mkChannelsRequest,
    mkChannelRequest,
    mkUsersRequest,
  )
where

import Control.Monad (when)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Types (Query)
import Network.Pusher.Data (Pusher (..))
import Network.Pusher.Error (PusherError (..))
import Network.Pusher.Internal.Auth (makeQS)
import Network.Pusher.Internal.HTTP
  ( RequestParams (RequestParams),
  )
import Network.Pusher.Protocol
  ( ChannelInfoQuery,
    ChannelsInfoQuery,
    toURLParam,
  )

mkTriggerRequest ::
  Pusher ->
  [T.Text] ->
  T.Text ->
  T.Text ->
  Maybe T.Text ->
  Word64 ->
  Either PusherError (RequestParams, A.Value)
mkTriggerRequest pusher chans event dat socketId timestamp = do
  when
    (length chans > 10)
    (Left $ PusherArgumentError "Must be less than 10 channels")
  let body =
        A.object $
          [ ("name", A.String event),
            ("channels", A.toJSON (map A.String chans)),
            ("data", A.String dat)
          ]
            ++ maybeToList (fmap (\sID -> ("socket_id", A.String sID)) socketId)
      bodyBS = BL.toStrict $ A.encode body
  when
    (B.length bodyBS > 10000)
    (Left $ PusherArgumentError "Body must be less than 10000 bytes long")
  return (mkPostRequest pusher "events" [] bodyBS timestamp, body)

mkChannelsRequest ::
  Pusher ->
  T.Text ->
  ChannelsInfoQuery ->
  Word64 ->
  RequestParams
mkChannelsRequest pusher prefixFilter attributes timestamp =
  let params =
        [ ("info", Just $ encodeUtf8 $ toURLParam attributes),
          ("filter_by_prefix", Just $ encodeUtf8 prefixFilter)
        ]
   in mkGetRequest pusher "channels" params timestamp

mkChannelRequest ::
  Pusher -> B.ByteString -> ChannelInfoQuery -> Word64 -> RequestParams
mkChannelRequest pusher chan attributes timestamp =
  let params = [("info", Just $ encodeUtf8 $ toURLParam attributes)]
      subPath = "channels/" <> chan
   in mkGetRequest pusher subPath params timestamp

mkUsersRequest :: Pusher -> B.ByteString -> Word64 -> RequestParams
mkUsersRequest pusher chan timestamp =
  let subPath = "channels/" <> chan <> "/users"
   in mkGetRequest pusher subPath [] timestamp

mkGetRequest ::
  Pusher ->
  B.ByteString ->
  Query ->
  Word64 ->
  RequestParams
mkGetRequest pusher subPath params timestamp =
  let host = pHost pusher
      port = pPort pusher
      path = pPath pusher <> subPath
      qs = makeQS (pToken pusher) "GET" path params "" timestamp
   in RequestParams host port path qs

mkPostRequest ::
  Pusher ->
  B.ByteString ->
  Query ->
  B.ByteString ->
  Word64 ->
  RequestParams
mkPostRequest pusher subPath params bodyBS timestamp =
  let host = pHost pusher
      port = pPort pusher
      path = pPath pusher <> subPath
      qs = makeQS (pToken pusher) "POST" path params bodyBS timestamp
   in RequestParams host port path qs
