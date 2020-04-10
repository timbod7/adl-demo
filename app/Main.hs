{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y

import ADL.Core(runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))
import ADL.Config(ServerConfig(..), Protocol(..))
import Control.Concurrent(threadDelay)
import System.Environment(getArgs)
import System.IO(stderr, hPutStrLn)
import System.Exit(exitFailure)

import Web.Spock
import Web.Spock.Config

-- | Read the contents of the specified file, parsing it
-- as a yaml serialised ADL value.
adlFromYamlFile :: AdlValue a => FilePath -> IO (Either T.Text a)
adlFromYamlFile file = (decodeAdlParseResult from . adlFromYamlByteString) <$> (LBS.readFile file)
  where
    adlFromYamlByteString :: (AdlValue a) => LBS.ByteString -> (ParseResult a)
    adlFromYamlByteString lbs = case Y.decodeEither' (LBS.toStrict lbs) of
      (Left e) -> ParseFailure ("Invalid yaml:" <> T.pack (Y.prettyPrintParseException e)) []
      (Right jv) -> runJsonParser jsonParser [] jv

    from = " from " <> T.pack file

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
  putStrLn ("Starting http server on port " ++ (show (sc_port sc)))
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ())
  runSpock (fromIntegral (sc_port sc)) (spock spockCfg app)

data MySession = EmptySession
data MyAppState = DummyAppState ()

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"


