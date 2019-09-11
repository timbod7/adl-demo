{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y

import ADL.Core(runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))
import ADL.Config(ServerConfig(..), Protocol(..))
import Control.Concurrent(threadDelay)
import System.Environment(lookupEnv)
import System.IO(stderr, hPutStrLn)
import System.Exit(exitFailure)

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
  let configEnvVar = "CONFIG_PATH"
  mEnvPath <- lookupEnv configEnvVar
  case mEnvPath of
    Nothing -> exitWithError (configEnvVar <> " not set in environment")
    (Just envPath) -> do
      eConfig <- adlFromYamlFile envPath
      case eConfig of
        (Left emsg) -> exitWithError (T.unpack emsg)
        (Right config) -> startServer config

exitWithError :: String -> IO ()
exitWithError emsg = do
  hPutStrLn stderr emsg
  exitFailure
  
startServer :: ServerConfig -> IO ()
startServer sc = do
  case sc_protocol sc of
    P_http -> putStrLn ("Starting http server on port " ++ (show (sc_port sc)))
    P_https{} -> putStrLn ("Starting https server on port " ++ (show (sc_port sc)))
  threadDelay 1000000000

