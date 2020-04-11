{-# LANGUAGE OverloadedStrings #-}
module ADL.Api(
    Api(..),
    Empty(..),
    mkApi,
    mkEmpty,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.TypeToken
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Prelude

data Api = Api
    { api_ping :: (ADL.Types.HttpPost Empty Empty)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkApi ::  Api
mkApi  = Api (ADL.Types.HttpPost "/ping" ADL.Types.HttpSecurity_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))

instance AdlValue Api where
    atype _ = "api.Api"
    
    jsonGen = genObject
        [ genField "ping" api_ping
        ]
    
    jsonParser = Api
        <$> parseFieldDef "ping" (ADL.Types.HttpPost "/ping" ADL.Types.HttpSecurity_public (ADL.Core.TypeToken.TypeToken) (ADL.Core.TypeToken.TypeToken))

data Empty = Empty
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkEmpty ::  Empty
mkEmpty  = Empty 

instance AdlValue Empty where
    atype _ = "api.Empty"
    
    jsonGen = genObject []
    jsonParser = Prelude.pure Empty