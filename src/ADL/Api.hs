{-# LANGUAGE OverloadedStrings #-}
module ADL.Api(
    Api(..),
    Empty(..),
    Jwt,
    LoginReq(..),
    Message(..),
    mkApi,
    mkEmpty,
    mkLoginReq,
    mkMessage,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.TypeToken
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data Api = Api
    { api_ping :: (ADL.Types.HttpPost Empty Empty)
    , api_login :: (ADL.Types.HttpPost LoginReq Jwt)
    , api_newMessage :: (ADL.Types.HttpPost Message Empty)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkApi ::  Api
mkApi  = Api (ADL.Types.HttpPost "/ping" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (ADL.Types.HttpPost "/login" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)) (ADL.Types.HttpPost "/new-message" ADL.Types.HS_token (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))

instance AdlValue Api where
    atype _ = "api.Api"
    
    jsonGen = genObject
        [ genField "ping" api_ping
        , genField "login" api_login
        , genField "newMessage" api_newMessage
        ]
    
    jsonParser = Api
        <$> parseFieldDef "ping" (ADL.Types.HttpPost "/ping" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "login" (ADL.Types.HttpPost "/login" ADL.Types.HS_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))
        <*> parseFieldDef "newMessage" (ADL.Types.HttpPost "/new-message" ADL.Types.HS_token (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))

data Empty = Empty
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkEmpty ::  Empty
mkEmpty  = Empty 

instance AdlValue Empty where
    atype _ = "api.Empty"
    
    jsonGen = genObject []
    jsonParser = Prelude.pure Empty

type Jwt = T.Text

data LoginReq = LoginReq
    { lr_email :: T.Text
    , lr_password :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkLoginReq :: T.Text -> T.Text -> LoginReq
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
    { m_body :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkMessage :: T.Text -> Message
mkMessage body = Message body

instance AdlValue Message where
    atype _ = "api.Message"
    
    jsonGen = genObject
        [ genField "body" m_body
        ]
    
    jsonParser = Message
        <$> parseField "body"