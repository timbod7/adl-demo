{-# LANGUAGE OverloadedStrings #-}
module Utils where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y

import Data.String(fromString)
import Web.Spock

import ADL.Core(adlToJson, runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))
import ADL.Types(HttpPost(..))
import Network.HTTP.Types.Status(status400)

-- | Read the contents of the specified file, parsing it
-- as a yaml serialised ADL value.
adlFromYamlFile :: AdlValue a => FilePath -> IO (Either T.Text a)
adlFromYamlFile file = (decodeAdlParseResult from . adlFromYamlByteString) <$> (LBS.readFile file)
  where
    from = " from " <> T.pack file

adlFromYamlByteString :: (AdlValue a) => LBS.ByteString -> (ParseResult a)
adlFromYamlByteString lbs = case Y.decodeEither' (LBS.toStrict lbs) of
  (Left e) -> ParseFailure ("Invalid yaml:" <> T.pack (Y.prettyPrintParseException e)) []
  (Right jv) -> runJsonParser jsonParser [] jv

adlPost :: (AdlValue i, AdlValue o) => ADL.Types.HttpPost i o
        -> (i -> ActionCtxT ctx (WebStateM conn sess st) o)
        -> SpockCtxM ctx conn sess st ()
adlPost postmeta handler = do
  post (fromString (T.unpack (hp_path postmeta))) $ do
    mjv <- jsonBody
    case (mjv :: Maybe Y.Value) of
      Nothing -> setStatus status400 >> text "json body not well formed"
      (Just jv) -> do
        let pv = runJsonParser jsonParser [] jv
        case decodeAdlParseResult " from post body " pv of
          (Left e) -> setStatus status400 >> text e
          (Right i) -> do
            o <- handler i
            json (adlToJson o)
