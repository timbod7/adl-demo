{-# LANGUAGE OverloadedStrings #-}
module Utils where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y

import Data.String(fromString)
import Web.JWT(decodeAndVerifySignature, hmacSecret)
import Web.Spock

import ADL.Core(adlToJson, runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))
import ADL.Types(HttpPost(..), HttpSecurity(..))
import Network.HTTP.Types.Status(status400, status401, Status)

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

class HasJwtSecret a where
  getJwtSecret :: a -> T.Text

adlPost :: (AdlValue i, AdlValue o, HasJwtSecret st) => ADL.Types.HttpPost i o
        -> (i -> ActionCtxT ctx (WebStateM conn sess st) (HandlerResponse o))
        -> SpockCtxM ctx conn sess st ()
adlPost postmeta handler = do

  post (fromString (T.unpack (hp_path postmeta))) $ do
    -- Perform authorization check
    authErr <- case hp_security postmeta of
      HS_public -> return Nothing
      HS_token -> do
        st <- getState
        checkBearerToken (getJwtSecret st)
  
    case authErr of
      (Just err) -> setStatus status401 >> text err
      Nothing -> do
        mjv <- jsonBody
        case (mjv :: Maybe Y.Value) of
          Nothing -> setStatus status400 >> text "json body not well formed"
          (Just jv) -> do
            let pv = runJsonParser jsonParser [] jv
            case decodeAdlParseResult " from post body " pv of
              (Left e) -> setStatus status400 >> text e
              (Right i) -> do
                hr <- handler i
                case hr of
                  (Success o) -> json (adlToJson o)
                  Other -> return ()

getBearerToken:: ActionCtxT ctx (WebStateM conn sess st) (Either T.Text T.Text)
getBearerToken = do
  mauth <- header "Authorization"
  case mauth of
    Nothing -> return (Left "no Authorization header")
    (Just auth) -> case T.words auth of
      ["Bearer",jwtText] -> return (Right jwtText)
      _ -> do
        return (Left "bad Authorization header format")

checkBearerToken:: T.Text -> ActionCtxT ctx (WebStateM conn sess st) (Maybe T.Text)
checkBearerToken jwtSecret = do
  eJwtToken <- getBearerToken
  case eJwtToken of
    Left err -> return (Just err)
    Right jwtText -> do
      case decodeAndVerifySignature (hmacSecret jwtSecret) jwtText of
        Nothing -> return (Just "invalid jwt")
        Just _ -> return Nothing

data HandlerResponse o = Success o | Other
