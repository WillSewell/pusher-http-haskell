{-# LANGUAGE FlexibleContexts #-}

module Pusher
  ( trigger
  , channels
  , channel
  , users
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Data.Pusher (Pusher(..))
import Pusher.Auth (makeQS)
import Pusher.HTTP (get, post)
import Pusher.Protocol
  ( ChannelInfo
  , ChannelInfoQuery
  , ChannelsInfo
  , ChannelsInfoQuery
  , Users
  , toURLParam
  )
import Pusher.Util (getIntPOSIXTime)

trigger
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => [T.Text] -> T.Text -> T.Text -> Maybe T.Text -> m ()
trigger channelNames event dat socketId = do
  when
    (length channelNames > 10)
    (throwError "Must be less than 10 channels")

  let
    body = A.object $
      [ ("name", A.String event)
      , ("channels", A.toJSON (map A.String channelNames))
      , ("data", A.String dat)
      ] ++ maybeToList (fmap (\sID ->  ("socket_id", A.String sID)) socketId)
    bodyBS = BL.toStrict $ A.encode body
  when
    (B.length bodyBS > 10000)
    (throwError "Body must be less than 10000KB")

  (ep, path) <- getEndpoint "events"
  qs <- makeQSWithTS "POST" path [] bodyBS
  post ep qs body

channels
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => T.Text -> ChannelsInfoQuery -> m ChannelsInfo
channels prefix attributes = do
  let
    params =
      [ ("info", encodeUtf8 $ toURLParam attributes)
      , ("filter_by_prefix", encodeUtf8 prefix)
      ]
  (ep, path) <- getEndpoint "channels"
  qs <- makeQSWithTS "GET" path params ""
  get ep qs

channel
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => T.Text -> ChannelInfoQuery -> m ChannelInfo
channel channelName attributes = do
  let params = [("info", encodeUtf8 $ toURLParam attributes)]
  (ep, path) <- getEndpoint $ "channels/" <> channelName
  qs <- makeQSWithTS "GET" path params ""
  get ep qs

users
 :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
 => T.Text -> m Users
users channelName = do
  (ep, path) <- getEndpoint $ "channels/" <> channelName <> "/users"
  qs <- makeQSWithTS "GET" path [] ""
  get ep qs

getEndpoint :: (MonadReader Pusher m) => T.Text -> m (T.Text, T.Text)
getEndpoint subPath = do
  host <- asks pusher'host
  path <- asks pusher'path
  let
    fullPath = path <> subPath
    endpoint = host <> fullPath
  return (endpoint, fullPath)

makeQSWithTS
  :: (MonadReader Pusher m, MonadIO m)
  => T.Text
  -> T.Text
  -> [(T.Text, B.ByteString)]
  -> B.ByteString
  -> m [(T.Text, B.ByteString)]
makeQSWithTS method path params body =
  makeQS method path params body =<< getIntPOSIXTime
