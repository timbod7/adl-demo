{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    ServerConfig(..),
    mkServerConfig,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data ServerConfig = ServerConfig
    { sc_port :: Data.Int.Int32
    , sc_jwtSecret :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkServerConfig :: T.Text -> ServerConfig
mkServerConfig jwtSecret = ServerConfig 8080 jwtSecret

instance AdlValue ServerConfig where
    atype _ = "config.ServerConfig"
    
    jsonGen = genObject
        [ genField "port" sc_port
        , genField "jwtSecret" sc_jwtSecret
        ]
    
    jsonParser = ServerConfig
        <$> parseFieldDef "port" 8080
        <*> parseField "jwtSecret"