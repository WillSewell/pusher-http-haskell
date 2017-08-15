module Webhook where

import           Data.Maybe (fromJust)
import           Data.Time.Clock.POSIX
import           Network.Pusher (parseChannel, AppKey, AppSecret, WebhookPayload(..), Webhooks(..),WebhookEv(..), parseWebhookPayloadReq)
import           Network.URI
import           Test.Hspec (Spec, describe, it)
import           Test.QuickCheck (property)
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Server as HTTP

data TestWebhookPayload = TestWebhookPayload
  {_webhookRequest :: HTTP.Request B.ByteString -- ^ A Request recieved from Pusher
  ,_hasKey         :: AppKey                    -- ^ Must have this key
  ,_hasSecret      :: AppSecret                 -- ^ Which must correspond to this secret
  ,_payload        :: Maybe WebhookPayload      -- ^ And which must parse to this Payload
  }

channelOccupiedPayload :: TestWebhookPayload
channelOccupiedPayload = TestWebhookPayload
 {_webhookRequest = HTTP.Request
      {HTTP.rqURI     = fromJust . parseURI $ "somewhere"
      ,HTTP.rqMethod  = HTTP.POST
      ,HTTP.rqHeaders =
          [HTTP.Header (HTTP.HdrCustom "X-Pusher-Key") "ebc2cca5d18f3cf01d99"
          ,HTTP.Header (HTTP.HdrCustom "X-Pusher-Signature") "4b3d29966e4930d875ec01012e37c18070f4b779b09f71af99d1f0baaffabc98"
          ]
      ,HTTP.rqBody    = "{\"time_ms\":1502790365001,\"events\":[{\"channel\":\"foo\",\"name\":\"channel_occupied\"}]}"
      }
 ,_hasKey            = "ebc2cca5d18f3cf01d99"
 ,_hasSecret         = "6f87cba29d7b8f6f4a36"
 ,_payload = Just WebhookPayload
     {xPusherKey       = "ebc2cca5d18f3cf01d99"
     ,xPusherSignature = "4b3d29966e4930d875ec01012e37c18070f4b779b09f71af99d1f0baaffabc98"
     ,webhooks         = Webhooks
         {timeMs     = posixSecondsToUTCTime 1502790365001
         ,webhookEvs =
             [ChannelOccupiedEv
                 {onChannel = parseChannel "foo"
                 }
             ]
         }
     }
 }

channelVacatedPayload :: TestWebhookPayload
channelVacatedPayload = TestWebhookPayload
 {_webhookRequest = HTTP.Request
      {HTTP.rqURI     = fromJust . parseURI $ "somewhere"
      ,HTTP.rqMethod  = HTTP.POST
      ,HTTP.rqHeaders =
          [HTTP.Header (HTTP.HdrCustom "X-Pusher-Key") "ebc2cca5d18f3cf01d99"
          ,HTTP.Header (HTTP.HdrCustom "X-Pusher-Signature") "c9c70dcf19e011912ecdabe8997b451a95667157d00e37c7476491e7f233c416"
          ]
      ,HTTP.rqBody    = "{\"time_ms\":1502790363928,\"events\":[{\"channel\":\"foo\",\"name\":\"channel_vacated\"}]}"
      }
 ,_hasKey            = "ebc2cca5d18f3cf01d99"
 ,_hasSecret         = "6f87cba29d7b8f6f4a36"
 ,_payload = Just WebhookPayload
     {xPusherKey       = "ebc2cca5d18f3cf01d99"
     ,xPusherSignature = "c9c70dcf19e011912ecdabe8997b451a95667157d00e37c7476491e7f233c416"
     ,webhooks         = Webhooks
         {timeMs     = posixSecondsToUTCTime 1502790363928
         ,webhookEvs =
             [ChannelVacatedEv
                 {onChannel = parseChannel "foo"
                 }
             ]
         }
     }
 }

test :: Spec
test = do
  describe "Webhook.parseWebhookPayloadReq" $ do
    it "A channel_occupied payload parses and validates" $ property $
      let TestWebhookPayload req hasKey correspondingSecret expectedPayload = channelOccupiedPayload
          parseResult = parseWebhookPayloadReq req (\k -> if k == hasKey
                                                            then Just correspondingSecret
                                                            else Nothing
                                                   )
         in parseResult == expectedPayload


    it "A channel_vacated payload parses and validates" $ property $
       let TestWebhookPayload req hasKey correspondingSecret expectedPayload = channelVacatedPayload
           parseResult = parseWebhookPayloadReq req (\k -> if k == hasKey
                                                             then Just correspondingSecret
                                                             else Nothing
                                                    )
         in parseResult == expectedPayload

