{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    ServerConfig(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Int
import qualified Data.Proxy
import qualified Prelude

data ServerConfig = ServerConfig
    { sc_port :: Data.Int.Int32
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkServerConfig :: Data.Int.Int32 -> ServerConfig
mkServerConfig port = ServerConfig port

instance AdlValue ServerConfig where
    atype _ = "config.ServerConfig"
    
    jsonGen = genObject
        [ genField "port" sc_port
        ]
    
    jsonParser = ServerConfig
        <$> parseField "port"