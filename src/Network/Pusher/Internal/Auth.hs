{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Network.Pusher.Internal.Auth
-- Description : Functions to perform authentication (generate auth signatures)
-- Copyright   : (c) Will Sewell, 2016
-- Licence     : MIT
-- Maintainer  : me@willsewell.com
-- Stability   : stable
--
-- This module contains helper functions for authenticating HTTP requests, as
-- well as publicly facing functions for authentication private and presence
-- channel users; these functions are re-exported in the main Pusher module.
module Network.Pusher.Internal.Auth
  ( authenticatePresence,
    authenticatePresenceWithEncoder,
    authenticatePrivate,
    makeQS,
  )
where

import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.Bifunctor (first)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Word (Word64)
import GHC.Exts (sortWith)
import Network.HTTP.Types (Query)
import Network.Pusher.Data (Token (..))
import Network.Pusher.Internal.Util (show')

-- | Generate the required query string parameters required to send API requests
--  to Pusher.
makeQS ::
  Token ->
  B.ByteString ->
  B.ByteString ->
  -- | Any additional parameters.
  Query ->
  B.ByteString ->
  Word64 ->
  Query
makeQS token method path params body timestamp =
  -- Generate all required parameters and add them to the list of existing ones
  -- Parameters are:
  -- - In alphabetical order
  -- - Keys are lower case
  let allParams =
        alphabeticalOrder . lowercaseKeys . (params ++) $
          [ ("auth_key", Just $ tokenKey token),
            ("auth_timestamp", Just $ show' timestamp),
            ("auth_version", Just "1.0"),
            ( "body_md5",
              Just
                $ B16.encode
                $ BA.convert (Hash.hash body :: Hash.Digest Hash.MD5)
            )
          ]
      -- Generate the auth signature from the list of parameters
      authSig =
        authSignature (tokenSecret token) $
          B.intercalate
            "\n"
            [method, path, formQueryString allParams]
   in -- Add the auth string to the list
      (("auth_signature", Just authSig) : allParams)
  where
    alphabeticalOrder = sortWith fst
    lowercaseKeys = map (first (BC.map toLower))

-- | Render key-value tuple mapping of query string parameters into a string.
formQueryString :: Query -> B.ByteString
formQueryString = B.intercalate "&" . map formQueryItem
  where
    formQueryItem (k, Just v) = k <> "=" <> v
    formQueryItem (k, Nothing) = k

-- | Create a Pusher auth signature of a string using the provided credentials.
authSignature :: B.ByteString -> B.ByteString -> B.ByteString
authSignature appSecret authString =
  B16.encode $
    BA.convert (HMAC.hmac appSecret authString :: HMAC.HMAC Hash.SHA256)

-- | Generate an auth signature of the form "app_key:auth_sig" for a user of a
--  private channel.
authenticatePrivate :: Token -> T.Text -> T.Text -> B.ByteString
authenticatePrivate token socketID channel =
  let sig =
        authSignature
          (tokenSecret token)
          (encodeUtf8 $ socketID <> ":" <> channel)
   in tokenKey token <> ":" <> sig

-- | Generate an auth signature of the form "app_key:auth_sig" for a user of a
--  presence channel.
authenticatePresence ::
  A.ToJSON a => Token -> T.Text -> T.Text -> a -> B.ByteString
authenticatePresence =
  authenticatePresenceWithEncoder
    (TL.toStrict . TL.toLazyText . A.encodeToTextBuilder . A.toJSON)

-- | As above, but allows the encoder of the user data to be specified. This is
--  useful for testing because the encoder can be mocked; aeson's encoder enodes
--  JSON object fields in arbitrary orders, which makes it impossible to test.
authenticatePresenceWithEncoder ::
  -- | The encoder of the user data.
  (a -> T.Text) ->
  Token ->
  T.Text ->
  T.Text ->
  a ->
  B.ByteString
authenticatePresenceWithEncoder userEncoder token socketID channel userData =
  let authString =
        encodeUtf8 $
          socketID <> ":" <> channel <> ":" <> userEncoder userData
      sig = authSignature (tokenSecret token) authString
   in tokenKey token <> ":" <> sig
