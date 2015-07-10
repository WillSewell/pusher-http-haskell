{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module HTTP where

import Control.Applicative (Applicative)
import Control.Monad.Error (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Test.Hspec (Spec, describe, it, shouldBe)
import Network.HTTP.Client (createCookieJar)
import Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
import Network.HTTP.Types.Status (mkStatus)
import Network.HTTP.Types.Version (http11)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM

import Pusher.HTTP
  ( MonadHTTP(postWith, getWith)
  , get
  , post
  )

-- Need to use newtype, otherwise there would be overlapping instances with the
-- existing ReaderT insatnce of MonadHTTP
-- Unfortunately most of boilerplate lines below are require because of this
-- newtype instance
newtype MockServer m a = MockServer
  { server :: ReaderT (Response BL.ByteString) m a }
  deriving (Applicative, Functor, Monad, MonadTrans)

runMockServer :: MockServer m a -> Response BL.ByteString -> m a
runMockServer (MockServer s) = runReaderT s

deriving instance Monad m => MonadReader (Response BL.ByteString) (MockServer m)

liftCatchMockerServer
  :: (m a -> (e -> m a) -> m a)
  -> MockServer m a
  -> (e -> MockServer m a)
  -> MockServer m a
liftCatchMockerServer catcher run handler =
  MockServer $ Reader.liftCatch catcher (server run) (server . handler)

instance (MonadError e m) => MonadError e (MockServer m) where
  throwError = lift . throwError
  catchError = liftCatchMockerServer catchError

instance Monad m => MonadHTTP (MockServer m) where
  getWith _ _ = ask
  postWith _ _ _ = ask

succeededResponse :: Response BL.ByteString
succeededResponse = Response
  { responseStatus = mkStatus 200 "succeess"
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = "{\"data\":\"some body\"}"
  , responseCookieJar = createCookieJar []
  , responseClose' = ResponseClose (return () :: IO ())
  }

failedResponse :: Response BL.ByteString
failedResponse = Response
  { responseStatus = mkStatus 404 "fail"
  , responseVersion = http11
  , responseHeaders = []
  , responseBody = ""
  , responseCookieJar = createCookieJar []
  , responseClose' = ResponseClose (return () :: IO ())
  }

test :: Spec
test = do
  describe "HTTP.get" $ do
    it "returns the body when the request is 200" $
      runMockServer (get "some/path" []) succeededResponse
      `shouldBe` (Right $ A.Object $ HM.singleton "data" (A.String "some body"))

    it "returns an error when the request fails" $
      (runMockServer (get "some/path" []) failedResponse :: Either String ())
      `shouldBe` Left "\"fail\""

  describe "HTTP.post" $ do
    it "returns the body when the request is 200" $
      -- TODO: Need a way of checking the POST data that is sent to the server
      runMockServer (post "some/path" [] (A.Object HM.empty)) succeededResponse
      `shouldBe` Right ()

    it "returns an error when the request fails" $
      (runMockServer
        (post "some/path" [] (A.Object HM.empty))
        failedResponse
        :: Either String ())
      `shouldBe` Left "\"fail\""
