{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Pusher where

import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Lens ((&), (^.), (.~))
import Control.Monad (sequence, when)
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

data Pusher = Pusher
  { pusher'endpoint :: T.Text
  , pusher'credentials :: Credentials
  }

data Credentials = Credentials
  { credentials'appID :: Integer
  , credentials'appKey :: B.ByteString
  , credentials'appSecret :: B.ByteString
  }

type PusherT = ReaderT Pusher IO

trigger
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => [T.Text] -> T.Text -> T.Text -> Maybe T.Text -> m ()
trigger channels event dat socketId = do
  when
    (length channels > 10)
    (throwError "Must be less than 10 channels")
  ep <- asks pusher'endpoint
  appID <- asks (credentials'appID . pusher'credentials)
  let
    body = A.object $
      [ ("name", A.String event)
      , ("channels", A.toJSON (map A.String channels))
      , ("data", A.String dat)
      ] ++ (maybeToList $ fmap (\sID ->  ("socket_id", A.String sID)) socketId)
    bodyBS = BL.toStrict $ A.encode body
  when
    (B.length bodyBS > 10000)
    (throwError "Body must be less than 10000KB")
  qs <- makeQS
    "POST"
    ("/apps/" <> T.pack (show appID) <> "/events")
    bodyBS
  params <- either
    (throwError . show)
    return
    (sequence $ (map (\(k, v) -> (k,) <$> decodeUtf8' v) qs))
  let opts = W.defaults & W.params .~ params
  r <- liftIO $ W.postWith opts (T.unpack $ ep <> "events") body
  let status = r ^. W.responseStatus
  if (status ^. W.statusCode == 200) then
     return ()
  else
     throwError $ show $ status ^. W.statusMessage

makeQS
  :: (MonadError String m, MonadReader Pusher m, MonadIO m, Functor m)
  => T.Text -> T.Text -> B.ByteString -> m [(T.Text, B.ByteString)]
makeQS method path body = do
  cred <- asks pusher'credentials
  ts <- BC.pack . show . (round :: POSIXTime -> Integer) <$> liftIO getPOSIXTime
  let
    params = sortWith fst $
      [ ("auth_key", credentials'appKey cred)
      , ("auth_timestamp", ts)
      , ("auth_version", "1.0")
      , ("body_md5", B16.encode (MD5.hash body))
      ]
    authSig = authSignature cred $ B.intercalate "\n" $
      [ encodeUtf8 method
      , encodeUtf8 path
      , formQueryString params
      ]
  return $ ("auth_signature", authSig) : params

formQueryString :: [(T.Text, B.ByteString)] -> B.ByteString
formQueryString =
  B.intercalate "&" . map (\(a, b) -> (encodeUtf8 a) <> "=" <> b)

authSignature :: Credentials -> B.ByteString -> B.ByteString
authSignature cred authString =
  B16.encode (HMAC.hmac SHA256.hash 64 (credentials'appSecret cred) authString)
