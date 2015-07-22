{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Network.Pusher.Auth
Description : Functions to perform authentication (generate auth signatures)
Copyright   : (c) Will Sewell, 2015
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

This module contains helper functions for authenticating HTTP requests, as well
as publically facing functions for authentication private and presence channel
users; these functions are re-exported in the main Pusher module.
-}
module Network.Pusher.Auth
  ( authenticatePresence
  , authenticatePresenceWithEncoder
  , authenticatePrivate, makeQS
  ) where

import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
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

import Data.Pusher (Credentials(..))

-- |Generate the required query string parameters required to send API requests
-- to Pusher.
makeQS
  :: B.ByteString
  -> B.ByteString
  -> T.Text
  -> T.Text
  -> [(B.ByteString, B.ByteString)] -- ^Any additional parameters
  -> B.ByteString
  -> Int -- ^Current UNIX timestamp
  -> [(B.ByteString, B.ByteString)]
makeQS appKey appSecret method path params body ts =
  let
    -- Generate all required parameters and add them to the list of existing ones
    allParams = sortWith fst $ params ++
      [ ("auth_key", appKey)
      , ("auth_timestamp", BC.pack $ show ts)
      , ("auth_version", "1.0")
      , ("body_md5", B16.encode (MD5.hash body))
      ]
    -- Generate the auth signature from the list of parameters
    authSig = authSignature appSecret $ B.intercalate "\n"
      [ encodeUtf8 method
      , encodeUtf8 path
      , formQueryString allParams
      ]
  in
    -- Add the auth string to the list
    ("auth_signature", authSig) : allParams

-- |Render key-value tuple mapping of query string parameters into a string.
formQueryString :: [(B.ByteString, B.ByteString)] -> B.ByteString
formQueryString =
  B.intercalate "&" . map (\(a, b) -> a <> "=" <> b)

-- |Create a Pusher auth signature of a string using the provided credentials.
authSignature :: B.ByteString -> B.ByteString -> B.ByteString
authSignature appSecret authString =
  B16.encode $ HMAC.hmac SHA256.hash 64 appSecret authString

-- |Generate an auth signature of the form "app_key:auth_sig" for a user of a
-- private channel.
authenticatePrivate
  :: Credentials -> B.ByteString -> B.ByteString -> B.ByteString
authenticatePrivate cred socketID channelName =
  let
    sig = authSignature
      (credentialsAppSecret cred)
      (socketID <> ":" <> channelName)
  in
    credentialsAppKey cred <> ":" <> sig

-- |Generate an auth signature of the form "app_key:auth_sig" for a user of a
-- presence channel.
authenticatePresence
  :: A.ToJSON a
  => Credentials -> B.ByteString -> B.ByteString -> a -> B.ByteString
authenticatePresence =
  authenticatePresenceWithEncoder (BL.toStrict . A.encode)

-- |As above, but allows the encoder of the user data to be specified. This is
-- useful for testing because the encoder can be mocked; aeson's encoder enodes
-- JSON object fields in arbitrary orders, which makes it impossible to test.
authenticatePresenceWithEncoder
  :: A.ToJSON a
  => (a -> B.ByteString) -- ^The encoder of the user data.
  -> Credentials
  -> B.ByteString
  -> B.ByteString
  -> a
  -> B.ByteString
authenticatePresenceWithEncoder userEncoder cred socketID channelName userData =
  let
    sig = authSignature (credentialsAppSecret cred)
      ( socketID <> ":"
      <> channelName <> ":"
      <> userEncoder userData
      )
  in
    credentialsAppKey cred <> ":" <> sig

