{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    FilePath,
    LogLevel(..),
    Protocol(..),
    ServerConfig(..),
    SslConfiguration(..),
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

type FilePath = T.Text

data LogLevel
    = LogLevel_error
    | LogLevel_warn
    | LogLevel_info
    | LogLevel_debug
    | LogLevel_trace
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue LogLevel where
    atype _ = "config.LogLevel"
    
    jsonGen = genUnion (\jv -> case jv of
        LogLevel_error -> genUnionVoid "error"
        LogLevel_warn -> genUnionVoid "warn"
        LogLevel_info -> genUnionVoid "info"
        LogLevel_debug -> genUnionVoid "debug"
        LogLevel_trace -> genUnionVoid "trace"
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "error" -> parseUnionVoid LogLevel_error
        "warn" -> parseUnionVoid LogLevel_warn
        "info" -> parseUnionVoid LogLevel_info
        "debug" -> parseUnionVoid LogLevel_debug
        "trace" -> parseUnionVoid LogLevel_trace
        _ -> parseFail "expected a discriminator for LogLevel (error,warn,info,debug,trace)" 

data Protocol
    = P_http
    | P_https SslConfiguration
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Protocol where
    atype _ = "config.Protocol"
    
    jsonGen = genUnion (\jv -> case jv of
        P_http -> genUnionVoid "http"
        P_https v -> genUnionValue "https" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "http" -> parseUnionVoid P_http
        "https" ->  parseUnionValue P_https
        _ -> parseFail "expected a discriminator for Protocol (http,https)" 

data ServerConfig = ServerConfig
    { sc_port :: Data.Int.Int32
    , sc_protocol :: Protocol
    , sc_logLevel :: LogLevel
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkServerConfig :: Data.Int.Int32 -> ServerConfig
mkServerConfig port = ServerConfig port P_http LogLevel_info

instance AdlValue ServerConfig where
    atype _ = "config.ServerConfig"
    
    jsonGen = genObject
        [ genField "port" sc_port
        , genField "protocol" sc_protocol
        , genField "logLevel" sc_logLevel
        ]
    
    jsonParser = ServerConfig
        <$> parseField "port"
        <*> parseFieldDef "protocol" P_http
        <*> parseFieldDef "logLevel" LogLevel_info

data SslConfiguration = SslConfiguration
    { ssl_certificate :: FilePath
    , ssl_certificateKey :: FilePath
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkSslConfiguration :: FilePath -> FilePath -> SslConfiguration
mkSslConfiguration certificate certificateKey = SslConfiguration certificate certificateKey

instance AdlValue SslConfiguration where
    atype _ = "config.SslConfiguration"
    
    jsonGen = genObject
        [ genField "certificate" ssl_certificate
        , genField "certificateKey" ssl_certificateKey
        ]
    
    jsonParser = SslConfiguration
        <$> parseField "certificate"
        <*> parseField "certificateKey"