{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified ADL.Api as API

import ADL.Core(runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))
import ADL.Config(ServerConfig(..))
import Control.Concurrent(threadDelay)
import Control.Monad.IO.Class
import Network.HTTP.Types.Status(status401)
import System.Environment(getArgs)
import System.IO(stderr, hPutStrLn)
import System.Exit(exitFailure)
import Utils(adlFromYamlFile, adlPost, HandlerResponse(..), HasJwtSecret(..))
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
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (MyAppState sc)
  runSpock (fromIntegral (sc_port sc)) (spock spockCfg app)

data MySession = EmptySession
data MyAppState = MyAppState {
  mas_serverConfig :: ServerConfig
}

type MyHandler o = ActionCtxT () (WebStateM () MySession MyAppState) (HandlerResponse o)

instance HasJwtSecret MyAppState where
  getJwtSecret = sc_jwtSecret . mas_serverConfig

app :: SpockM () MySession MyAppState ()
app = do
  let api = API.mkApi
  adlPost (API.api_ping api) handlePing
  adlPost (API.api_login api) handleLogin
  adlPost (API.api_newMessage api) handleNewMessage

handlePing :: API.Empty -> MyHandler API.Empty
handlePing _ = return (Success API.Empty)

handleLogin :: API.LoginReq -> MyHandler API.Jwt
handleLogin API.LoginReq{API.lr_email, API.lr_password} =
  if lr_email == "test@email.com" && lr_password == "abcde"
    then do
      st <- getState
      let jwtSecret = sc_jwtSecret (mas_serverConfig st)
          header = mempty
          claims = mempty
          jwt = encodeSigned (hmacSecret jwtSecret) header claims
      return (Success jwt)
    else do
      setStatus status401
      return Other

handleNewMessage :: API.Message -> MyHandler API.Empty
handleNewMessage message = do
  -- do something with the message
  return (Success API.Empty)
