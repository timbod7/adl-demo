{-# LANGUAGE OverloadedStrings #-}
module Utils where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.Aeson as JS

import Control.Monad(when)
import Data.String(fromString)
import Web.JWT(claims, decodeAndVerifySignature, hmacSecret, ClaimsMap(..), JWTClaimsSet(..))
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

-- | Parse a ByteString containing yaml to an ADL value
adlFromYamlByteString :: (AdlValue a) => LBS.ByteString -> (ParseResult a)
adlFromYamlByteString lbs = case Y.decodeEither' (LBS.toStrict lbs) of
  (Left e) -> ParseFailure ("Invalid yaml:" <> T.pack (Y.prettyPrintParseException e)) []
  (Right jv) -> runJsonParser jsonParser [] jv

-- | Typeclass to abstract getting the Jwt signing secret out of the system state
class HasJwtSecret st where
  getJwtSecret :: st -> T.Text

-- | Add a spock route implementing an http post request, with the specification for
-- the request supplied as a value of type HttpPost.
--
-- Assuming a request body of type i, and a response body of type o, the resulting
-- handler implements JWT based authorization checks, and request and response parsing
-- and serialization.
adlPost :: (AdlValue i, AdlValue o, HasJwtSecret st)
        => HttpPost i o
        -> (i -> ActionCtxT (Maybe JWTClaimsSet) (WebStateM conn sess st) o)
        -> SpockCtxM ctx conn sess st ()
adlPost postmeta handler = prehook checkAuth $ post path runRequest
  where
    path = fromString (T.unpack (hp_path postmeta))

    checkAuth = do
      jwtSecret <- getJwtSecret <$> getState
      case hp_security postmeta of
        HS_public -> return Nothing
        HS_token -> Just <$> getVerifiedJwtClaims jwtSecret
        HS_adminToken -> do
          claims <- getVerifiedJwtClaims jwtSecret
          when (not (isAdmin claims)) $ do
            error401 "needs admin"
          return (Just claims)

    runRequest = do
      mjv <- jsonBody
      case mjv of
        Nothing -> error400 "json body not well formed"
        (Just jv) -> do
          let pv = runJsonParser jsonParser [] jv
          case decodeAdlParseResult " from post body " pv of
            Left e -> error400 e
            Right i -> do
              o <- handler i
              json (adlToJson o)

getVerifiedJwtClaims :: T.Text -> ActionCtxT ctx (WebStateM conn sess st) JWTClaimsSet
getVerifiedJwtClaims jwtSecret = do
  jwtText <- getBearerToken
  case decodeAndVerifySignature (hmacSecret jwtSecret) jwtText of
    Nothing -> error401 "invalid jwt"
    Just verifiedJwt -> return (claims verifiedJwt)

getBearerToken:: ActionCtxT ctx (WebStateM conn sess st) T.Text
getBearerToken = do
  mauth <- header "Authorization"
  case mauth of
    Nothing -> error401 "no Authorization header"
    (Just auth) -> case T.words auth of
      ["Bearer",jwtText] -> return jwtText
      _ -> error401 "bad Authorization header format"

error400 :: T.Text -> ActionCtxT ctx (WebStateM conn sess st) a
error400 err = setStatus status400 >> text err

error401 :: T.Text -> ActionCtxT ctx (WebStateM conn sess st) a
error401 err = setStatus status401 >> text err

isAdmin :: JWTClaimsSet -> Bool
isAdmin claims = case M.lookup "admin" (unClaimsMap (unregisteredClaims claims)) of
  (Just (JS.Bool v)) -> v
  _ -> False

emailFromClaims :: JWTClaimsSet -> T.Text
emailFromClaims claims = case M.lookup "email" (unClaimsMap (unregisteredClaims claims)) of
  (Just (JS.String v)) -> v
  _ -> "??"
