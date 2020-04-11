{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y

import ADL.Core(runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))
import ADL.Config(ServerConfig(..))
import ADL.Api(Api(..), mkApi, Empty(..))
import Control.Concurrent(threadDelay)
import Control.Monad.IO.Class
import System.Environment(getArgs)
import System.IO(stderr, hPutStrLn)
import System.Exit(exitFailure)
import Utils(adlFromYamlFile, adlPost)

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
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ())
  runSpock (fromIntegral (sc_port sc)) (spock spockCfg app)

data MySession = EmptySession
data MyAppState = DummyAppState ()
type MyHandler = ActionCtxT () (WebStateM () MySession MyAppState)

app :: SpockM () MySession MyAppState ()
app = do
  let api = mkApi
  adlPost (api_ping api) handlePing

handlePing :: Empty -> MyHandler Empty
handlePing _ = return Empty
