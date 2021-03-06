{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ADL.Types(
    Email,
    Empty(..),
    HttpPost(..),
    HttpSecurity(..),
    Jwt,
    Password,
    TimeStamp,
    UserId(..),
    mkEmpty,
    mkHttpPost,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import TimeStamp(TimeStamp)
import qualified ADL.Core.TypeToken
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type Email = T.Text

data Empty = Empty
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkEmpty ::  Empty
mkEmpty  = Empty 

instance AdlValue Empty where
    atype _ = "types.Empty"
    
    jsonGen = genObject []
    jsonParser = Prelude.pure Empty

data HttpPost i o = HttpPost
    { hp_path :: T.Text
    , hp_security :: HttpSecurity
    , hp_reqType :: (ADL.Core.TypeToken.TypeToken i)
    , hp_respType :: (ADL.Core.TypeToken.TypeToken o)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkHttpPost :: T.Text -> HttpSecurity -> HttpPost i o
mkHttpPost path security = HttpPost path security (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken)

instance (AdlValue i, AdlValue o) => AdlValue (HttpPost i o) where
    atype _ = T.concat
        [ "types.HttpPost"
        , "<", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy i)
        , ",", atype (Data.Proxy.Proxy :: Data.Proxy.Proxy o)
        , ">" ]
    
    jsonGen = genObject
        [ genField "path" hp_path
        , genField "security" hp_security
        , genField "reqType" hp_reqType
        , genField "respType" hp_respType
        ]
    
    jsonParser = HttpPost
        <$> parseField "path"
        <*> parseField "security"
        <*> parseFieldDef "reqType" (ADL.Core.TypeToken.TypeToken)
        <*> parseFieldDef "respType" (ADL.Core.TypeToken.TypeToken)

data HttpSecurity
    = HS_public
    | HS_token
    | HS_adminToken
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue HttpSecurity where
    atype _ = "types.HttpSecurity"
    
    jsonGen = genUnion (\jv -> case jv of
        HS_public -> genUnionVoid "public"
        HS_token -> genUnionVoid "token"
        HS_adminToken -> genUnionVoid "adminToken"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "public" -> parseUnionVoid HS_public
        "token" -> parseUnionVoid HS_token
        "adminToken" -> parseUnionVoid HS_adminToken
        _ -> parseFail "expected a discriminator for HttpSecurity (public,token,adminToken)" 

type Jwt = T.Text

type Password = T.Text


newtype UserId = UserId { unUserId :: T.Text }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue UserId where
    atype _ = "types.UserId"
    
    jsonGen = JsonGen (\(UserId v) -> adlToJson v)
    
    jsonParser = UserId <$> jsonParser