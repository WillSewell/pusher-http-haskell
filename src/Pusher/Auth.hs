{-# LANGUAGE FlexibleContexts #-}

module Pusher.Auth
  ( authenticatePresence
  , authenticatePresenceWithEncoder
  , authenticatePrivate, makeQS
  ) where

import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Reader (MonadReader, asks)
import GHC.Exts (sortWith)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.MAC.HMAC as HMAC

import Data.Pusher (Pusher(..), Credentials(..))

makeQS
  :: MonadReader Pusher m
  => T.Text
  -> T.Text
  -> [(T.Text, B.ByteString)]
  -> B.ByteString
  -> Int
  -> m [(T.Text, B.ByteString)]
makeQS method path params body ts = do
  cred <- asks pusher'credentials
  let
    allParams = sortWith fst $ params ++
      [ ("auth_key", credentials'appKey cred)
      , ("auth_timestamp", BC.pack $ show ts)
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

authenticatePrivate
  :: Credentials -> B.ByteString -> B.ByteString -> B.ByteString
authenticatePrivate cred socketID channelName =
  let
    sig = authSignature cred (socketID <> ":" <> channelName)
  in
    credentials'appKey cred <> ":" <> sig

authenticatePresence
  :: A.ToJSON a
  => Credentials -> B.ByteString -> B.ByteString -> a -> B.ByteString
authenticatePresence =
  authenticatePresenceWithEncoder (BL.toStrict . A.encode)

authenticatePresenceWithEncoder
  :: A.ToJSON a
  => (a -> B.ByteString)
  -> Credentials
  -> B.ByteString
  -> B.ByteString
  -> a
  -> B.ByteString
authenticatePresenceWithEncoder userEncoder cred socketID channelName userData =
  let
    sig = authSignature cred
      ( socketID <> ":"
      <> channelName <> ":"
      <> userEncoder userData
      )
  in
    credentials'appKey cred <> ":" <> sig

