{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified ADL.Api as API

import ADL.Core(runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))
import ADL.Config(ServerConfig(..))
import ADL.Types(Empty(..), Jwt)
import Control.Concurrent(threadDelay)
import Control.Concurrent.STM.TVar(newTVar,TVar)
import Control.Monad.IO.Class
import Control.Monad.STM(atomically)
import Network.HTTP.Types.Status(status401)
import Server(initAppState, serverApp, MySession(..))
import System.Environment(getArgs)
import System.IO(stderr, hPutStrLn)
import System.Exit(exitFailure)
import Utils(adlFromYamlFile, adlPost)
import Web.JWT(encodeSigned, hmacSecret)

import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> do
      eConfig <- adlFromYamlFile configPath
      case eConfig of
        (Left emsg) -> exitWithError (T.unpack emsg)
        (Right config) -> startServer config
    _ -> exitWithError "Usage: server <config.yaml>"

exitWithError :: String -> IO a
exitWithError emsg = do
  hPutStrLn stderr emsg
  exitFailure
  
startServer :: ServerConfig -> IO ()
startServer sc = do
  state <- initAppState sc
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase state
  runSpock (fromIntegral (sc_port sc)) (spock spockCfg serverApp)
