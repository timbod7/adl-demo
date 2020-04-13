{-# LANGUAGE OverloadedStrings #-}
module ADL.Api(
    Api(..),
    CreateUserReq(..),
    CreateUserResp(..),
    LoginReq(..),
    Message(..),
    NewMessageReq(..),
    RecentMessagesReq(..),
    mkApi,
    mkCreateUserReq,
    mkLoginReq,
    mkMessage,
    mkNewMessageReq,
    mkRecentMessagesReq,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.TypeToken
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Api = Api
    { api_ping :: (ADL.Types.HttpPost ADL.Types.Empty ADL.Types.Empty)
    , api_login :: (ADL.Types.HttpPost LoginReq ADL.Types.Jwt)
    , api_newMessage :: (ADL.Types.HttpPost NewMessageReq ADL.Types.Empty)
    , api_recentMessages :: (ADL.Types.HttpPost RecentMessagesReq [Message])
    , api_createUser :: (ADL.Types.HttpPost CreateUserReq CreateUserResp)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkApi ::  Api
mkApi  = Api (ADL.Types.HttpPost "/ping" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (ADL.Types.HttpPost "/login" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (ADL.Types.HttpPost "/new-message" ADL.Types.HS_token (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (ADL.Types.HttpPost "/recent-messages" ADL.Types.HS_token (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (ADL.Types.HttpPost "/create-user" ADL.Types.HS_adminToken (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))

instance AdlValue Api where
    atype _ = "api.Api"
    
    jsonGen = genObject
        [ genField "ping" api_ping
        , genField "login" api_login
        , genField "newMessage" api_newMessage
        , genField "recentMessages" api_recentMessages
        , genField "createUser" api_createUser
        ]
    
    jsonParser = Api
        <$> parseFieldDef "ping" (ADL.Types.HttpPost "/ping" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "login" (ADL.Types.HttpPost "/login" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "newMessage" (ADL.Types.HttpPost "/new-message" ADL.Types.HS_token (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "recentMessages" (ADL.Types.HttpPost "/recent-messages" ADL.Types.HS_token (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "createUser" (ADL.Types.HttpPost "/create-user" ADL.Types.HS_adminToken (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))

data CreateUserReq = CreateUserReq
    { cur_email :: ADL.Types.Email
    , cur_password :: ADL.Types.Password
    , cur_isAdmin :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkCreateUserReq :: ADL.Types.Email -> ADL.Types.Password -> Prelude.Bool -> CreateUserReq
mkCreateUserReq email password isAdmin = CreateUserReq email password isAdmin

instance AdlValue CreateUserReq where
    atype _ = "api.CreateUserReq"
    
    jsonGen = genObject
        [ genField "email" cur_email
        , genField "password" cur_password
        , genField "isAdmin" cur_isAdmin
        ]
    
    jsonParser = CreateUserReq
        <$> parseField "email"
        <*> parseField "password"
        <*> parseField "isAdmin"

data CreateUserResp
    = CUR_success
    | CUR_duplicateEmail
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue CreateUserResp where
    atype _ = "api.CreateUserResp"
    
    jsonGen = genUnion (\jv -> case jv of
        CUR_success -> genUnionVoid "success"
        CUR_duplicateEmail -> genUnionVoid "duplicateEmail"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "success" -> parseUnionVoid CUR_success
        "duplicateEmail" -> parseUnionVoid CUR_duplicateEmail
        _ -> parseFail "expected a discriminator for CreateUserResp (success,duplicateEmail)" 

data LoginReq = LoginReq
    { lr_email :: ADL.Types.Email
    , lr_password :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkLoginReq :: ADL.Types.Email -> T.Text -> LoginReq
mkLoginReq email password = LoginReq email password

instance AdlValue LoginReq where
    atype _ = "api.LoginReq"
    
    jsonGen = genObject
        [ genField "email" lr_email
        , genField "password" lr_password
        ]
    
    jsonParser = LoginReq
        <$> parseField "email"
        <*> parseField "password"

data Message = Message
    { m_id :: T.Text
    , m_postedBy :: ADL.Types.Email
    , m_postedAt :: ADL.Types.TimeStamp
    , m_body :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkMessage :: T.Text -> ADL.Types.Email -> ADL.Types.TimeStamp -> T.Text -> Message
mkMessage id postedBy postedAt body = Message id postedBy postedAt body

instance AdlValue Message where
    atype _ = "api.Message"
    
    jsonGen = genObject
        [ genField "id" m_id
        , genField "postedBy" m_postedBy
        , genField "postedAt" m_postedAt
        , genField "body" m_body
        ]
    
    jsonParser = Message
        <$> parseField "id"
        <*> parseField "postedBy"
        <*> parseField "postedAt"
        <*> parseField "body"

data NewMessageReq = NewMessageReq
    { nmr_body :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkNewMessageReq :: T.Text -> NewMessageReq
mkNewMessageReq body = NewMessageReq body

instance AdlValue NewMessageReq where
    atype _ = "api.NewMessageReq"
    
    jsonGen = genObject
        [ genField "body" nmr_body
        ]
    
    jsonParser = NewMessageReq
        <$> parseField "body"

data RecentMessagesReq = RecentMessagesReq
    { rmr_maxMessages :: Data.Int.Int32
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkRecentMessagesReq :: Data.Int.Int32 -> RecentMessagesReq
mkRecentMessagesReq maxMessages = RecentMessagesReq maxMessages

instance AdlValue RecentMessagesReq where
    atype _ = "api.RecentMessagesReq"
    
    jsonGen = genObject
        [ genField "maxMessages" rmr_maxMessages
        ]
    
    jsonParser = RecentMessagesReq
        <$> parseField "maxMessages"