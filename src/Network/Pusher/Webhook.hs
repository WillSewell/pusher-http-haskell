module Network.Pusher.Webhook
  (Webhooks(..)
  ,WebhookEv(..)
  )
  where

import           Data.Text (Text)
import           Data.Time (UTCTime(..))
import           Network.Pusher.Data (Channel(..),SocketID)
import           Network.Pusher.Protocol (User(..))
import qualified Data.Aeson as A


-- | A Webhook is received by POST request from Pusher to notify your server of
-- a number of 'WebhookEv'ents. Multiple events are received under the same
-- timestamp if batch events is enabled.
data Webhooks = Webhooks
  {timeMs :: UTCTime
  ,webhookEvs :: [WebhookEv]
  }


-- | A 'WebhookEv'ent is one of several events Pusher may send to your server
-- in response to events your users may trigger.
data WebhookEv
  -- | A Channel has become occupied. There is > 1 subscriber.
  = ChannelOccupiedEv
     {onChannel :: Channel
     }

  -- | A Channel has become vacated. There are 0 subscribers.
  | ChannelVacatedEv
     {onChannel :: Channel
     }
  
  -- | A new user has subscribed to a presence Channel.
  | MemberAddedEv
     {onChannel :: Channel
     ,withUser  :: User
     }

  -- | A user has unsubscribed from a presence Channel.
  | MemberRemovedEv
     {onChannel :: Channel
     ,withUser  :: User
     }

  -- | A client has sent a named client event with some json body. They have a
  -- SocketID and a User if they were in a presence Channel.
  | ClientEv
     {onChannel :: Channel
     ,clientEvName :: Text
     ,clientEvBody :: A.Value
     ,withSocketId :: SocketID
     ,withPossibleUser :: Maybe User
     }

