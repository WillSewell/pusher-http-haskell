-- |
-- Module      : Network.Pusher.Internal
-- Description : Pure functions called by the public interface
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : stable
module Network.Pusher.Internal
  ( mkTriggerRequest,
    mkChannelsRequest,
    mkChannelRequest,
    mkUsersRequest,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Types (Query)
import Network.Pusher.Data (Pusher (..))
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
  (RequestParams, A.Value)
mkTriggerRequest pusher channels event dat socketId timestamp =
  let body =
        A.object $
          [ ("name", A.String event),
            ("channels", A.toJSON (map A.String channels)),
            ("data", A.String dat)
          ]
            ++ maybeToList (fmap (\sID -> ("socket_id", A.String sID)) socketId)
      bodyBS = BL.toStrict $ A.encode body
   in (mkPostRequest pusher "events" [] bodyBS timestamp, body)

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
mkGetRequest pusher subPath params = mkRequest pusher "GET" subPath params ""

mkPostRequest ::
  Pusher ->
  B.ByteString ->
  Query ->
  B.ByteString ->
  Word64 ->
  RequestParams
mkPostRequest pusher = mkRequest pusher "POST"

mkRequest ::
  Pusher ->
  B.ByteString ->
  B.ByteString ->
  Query ->
  B.ByteString ->
  Word64 ->
  RequestParams
mkRequest pusher method subPath params bodyBS timestamp =
  let useTLS = pUseTLS pusher
      host = pHost pusher
      port = pPort pusher
      path = pPath pusher <> subPath
      qs = makeQS (pToken pusher) method path params bodyBS timestamp
   in RequestParams useTLS host port path qs
