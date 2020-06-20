{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Network.Pusher.Internal.Auth
Description : Functions to perform authentication (generate auth signatures)
Copyright   : (c) Will Sewell, 2016
Licence     : MIT
Maintainer  : me@willsewell.com
Stability   : experimental

This module contains helper functions for authenticating HTTP requests, as well
as publically facing functions for authentication private and presence channel
users; these functions are re-exported in the main Pusher module.
-}
module Network.Pusher.Internal.Auth
  ( AuthString
  , AuthSignature
  , authenticatePresence
  , authenticatePresenceWithEncoder
  , authenticatePrivate
  , makeQS
  ) where

import qualified Data.Aeson as A
import Data.Text.Encoding (encodeUtf8)
import GHC.Exts (sortWith)
import qualified Data.Aeson.Text as A
import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Word (Word64)

import Network.Pusher.Data
       (AppKey, AppSecret, Channel, Credentials(..), SocketID,
        renderChannel)
import Network.Pusher.Internal.HTTP (RequestQueryString)
import Network.Pusher.Internal.Util (show')

-- |Generate the required query string parameters required to send API requests
-- to Pusher.
makeQS ::
     AppKey
  -> AppSecret
  -> T.Text
  -> T.Text
  -> RequestQueryString -- ^Any additional parameters.
  -> B.ByteString
  -> Word64
  -> RequestQueryString
makeQS appKey appSecret method path params body timestamp =
  let allParams
    -- Generate all required parameters and add them to the list of existing
    -- ones
       =
        sortWith fst $
        params ++
        [ ("auth_key", appKey)
        , ("auth_timestamp", show' timestamp)
        , ("auth_version", "1.0")
        , ( "body_md5"
          , B16.encode $ BA.convert (Hash.hash body :: Hash.Digest Hash.MD5))
        ]
    -- Generate the auth signature from the list of parameters
      authSig =
        authSignature appSecret $
        B.intercalate
          "\n"
          [encodeUtf8 method, encodeUtf8 path, formQueryString allParams]
    -- Add the auth string to the list
  in ("auth_signature", authSig) : allParams

-- |Render key-value tuple mapping of query string parameters into a string.
formQueryString :: RequestQueryString -> B.ByteString
formQueryString = B.intercalate "&" . map (\(a, b) -> a <> "=" <> b)

-- |The bytestring to sign with the app secret to create a signature from.
type AuthString = B.ByteString

-- |A Pusher auth signature.
type AuthSignature = B.ByteString

-- |Create a Pusher auth signature of a string using the provided credentials.
authSignature :: AppSecret -> AuthString -> AuthSignature
authSignature appSecret authString =
  B16.encode $
  BA.convert (HMAC.hmac appSecret authString :: HMAC.HMAC Hash.SHA256)

-- |Generate an auth signature of the form "app_key:auth_sig" for a user of a
-- private channel.
authenticatePrivate :: Credentials -> SocketID -> Channel -> AuthSignature
authenticatePrivate cred socketID channel =
  let sig =
        authSignature
          (credentialsAppSecret cred)
          (encodeUtf8 $ socketID <> ":" <> renderChannel channel)
  in credentialsAppKey cred <> ":" <> sig

-- |Generate an auth signature of the form "app_key:auth_sig" for a user of a
-- presence channel.
authenticatePresence ::
     A.ToJSON a => Credentials -> SocketID -> Channel -> a -> AuthSignature
authenticatePresence =
  authenticatePresenceWithEncoder
    (TL.toStrict . TL.toLazyText . A.encodeToTextBuilder . A.toJSON)

-- |As above, but allows the encoder of the user data to be specified. This is
-- useful for testing because the encoder can be mocked; aeson's encoder enodes
-- JSON object fields in arbitrary orders, which makes it impossible to test.
authenticatePresenceWithEncoder ::
     (a -> T.Text) -- ^The encoder of the user data.
  -> Credentials
  -> SocketID
  -> Channel
  -> a
  -> AuthSignature
authenticatePresenceWithEncoder userEncoder cred socketID channel userData =
  let authString =
        encodeUtf8 $
        socketID <> ":" <> renderChannel channel <> ":" <> userEncoder userData
      sig = authSignature (credentialsAppSecret cred) authString
  in credentialsAppKey cred <> ":" <> sig
