{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Pusher where

import Data.Aeson ((.:))
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Applicative ((<$>), (<*>))
import Control.Lens ((&), (^.), (.~))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error (MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import GHC.Exts (sortWith)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.MAC.HMAC as HMAC
import qualified Network.Wreq as W
import qualified Network.Wreq.Types as WT

import Pusher.Protocol
  ( ChannelInfo
  , ChannelInfoQuery
  , ChannelsInfo
  , ChannelsInfoQuery
  , ToURLParam
  , toURLParam
  )
import Pusher.Util (failExpectObj)

data Pusher = Pusher
  { pusher'endpoint :: T.Text
  , pusher'path :: T.Text
  , pusher'credentials :: Credentials
  }

data Credentials = Credentials
  { credentials'appID :: Integer
  , credentials'appKey :: B.ByteString
  , credentials'appSecret :: B.ByteString
  }

instance A.FromJSON Credentials where
  parseJSON (A.Object v) = Credentials
    <$> v .: "app-id"
    <*> (encodeUtf8 <$> v .: "app-key")
    <*> (encodeUtf8 <$> v .: "app-secret")
  parseJSON v2 = failExpectObj v2

type PusherT = ReaderT Pusher IO

trigger
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => [T.Text] -> T.Text -> T.Text -> Maybe T.Text -> m ()
trigger channelNames event dat socketId = do
  when
    (length channelNames > 10)
    (throwError "Must be less than 10 channels")
  ep <- asks pusher'endpoint
  appID <- asks (credentials'appID . pusher'credentials)
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
  qs <- makeQS
    "POST"
    ("/apps/" <> T.pack (show appID) <> "/events")
    []
    bodyBS
  post (ep <> "events") qs body

channels
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => T.Text -> ChannelsInfoQuery -> m ChannelsInfo
channels prefix attributes = do
  ep <- asks pusher'endpoint
  appID <- asks (credentials'appID . pusher'credentials)
  qs <- makeQS
    "GET"
    ("/apps/" <> T.pack (show appID) <> "/channels")
    [ ("info", encodeUtf8 $ toURLParam attributes)
    , ("filter_by_prefix", encodeUtf8 prefix)
    ]
    ""
  get (ep <> "channels") qs

channel
  ::
    ( Functor m
    , MonadError String m
    , MonadIO m
    , MonadReader Pusher m
    )
  => T.Text -> ChannelInfoQuery -> m ChannelInfo
channel channelName attributes = do
  ep <- asks pusher'endpoint
  path <- asks pusher'path
  let
    subPath = "channels/" <> channelName
    fullPath = path <> subPath
  qs <- makeQS
    "GET"
    fullPath
    [("info", encodeUtf8 $ toURLParam attributes)]
    ""
  liftIO $ print qs
  get (ep <> subPath) qs

makeQS
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => T.Text -> T.Text -> [(T.Text, B.ByteString)] -> B.ByteString -> m [(T.Text, B.ByteString)]
makeQS method path params body = do
  cred <- asks pusher'credentials
  ts <- BC.pack . show . (round :: POSIXTime -> Integer) <$> liftIO getPOSIXTime
  let
    allParams = sortWith fst $ params ++
      [ ("auth_key", credentials'appKey cred)
      , ("auth_timestamp", ts)
      , ("auth_version", "1.0")
      , ("body_md5", B16.encode (MD5.hash body))
      ]
    authSig = authSignature cred $ B.intercalate "\n"
      [ encodeUtf8 method
      , encodeUtf8 path
      , formQueryString allParams
      ]
  return $ ("auth_signature", authSig) : allParams

formQueryString :: [(T.Text, B.ByteString)] -> B.ByteString
formQueryString =
  B.intercalate "&" . map (\(a, b) -> encodeUtf8 a <> "=" <> b)

authSignature :: Credentials -> B.ByteString -> B.ByteString
authSignature cred authString =
  B16.encode (HMAC.hmac SHA256.hash 64 (credentials'appSecret cred) authString)

get :: (A.FromJSON a, MonadError String m, MonadIO m) => T.Text -> [(T.Text, B.ByteString)] -> m a
get ep qs  = do
  params <- either
    (throwError . show)
    return
    (mapM (\(k, v) -> (k,) <$> decodeUtf8' v) qs)
  let opts = W.defaults & W.params .~ params
  r <- liftIO $ W.getWith opts (T.unpack ep)
  let status = r ^. W.responseStatus
  if status ^. W.statusCode == 200 then do
     liftIO $ print (r ^. W.responseBody)
     either throwError return (A.eitherDecode $ r ^. W.responseBody)
  else
     throwError $ show $ status ^. W.statusMessage

post :: (WT.Postable a, MonadError String m, MonadIO m) => T.Text -> [(T.Text, B.ByteString)] -> a -> m ()
post ep qs body = do
  params <- either
    (throwError . show)
    return
    (mapM (\(k, v) -> (k,) <$> decodeUtf8' v) qs)
  let opts = W.defaults & W.params .~ params
  r <- liftIO $ W.postWith opts (T.unpack ep) body
  let status = r ^. W.responseStatus
  unless
    (status ^. W.statusCode == 200)
    (throwError $ show $ status ^. W.statusMessage)
