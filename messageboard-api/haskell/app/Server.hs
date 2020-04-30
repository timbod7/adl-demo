{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Server where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Aeson as JS
import qualified ADL.Api as API

import ADL.Config(ServerConfig(..))
import ADL.Types(Empty(..), Jwt, UserId(..))
import Data.List(find)
import Data.Password(checkPass, mkPass, newSalt, hashPassWithSalt, Salt, PassCheck(..), PassHash(..))
import Data.Time.Clock(getCurrentTime)
import Control.Concurrent.STM.TVar(newTVar, readTVar, modifyTVar', TVar)
import Control.Monad.IO.Class
import Control.Monad.STM(atomically)
import Network.HTTP.Types.Status(status401)
import TimeStamp(TimeStamp(..))
import Utils(adlFromYamlFile, adlPost, error401, emailFromClaims, HasJwtSecret(..))
import Web.JWT(encodeSigned, hmacSecret, ClaimsMap(..), JWTClaimsSet(..))

import Web.Spock
import Web.Spock.Config

data MySession = EmptySession

data User = User {
  u_email :: T.Text,
  u_hashedPassword :: T.Text,
  u_isAdmin :: Bool
}

data MyAppState = MyAppState {
  mas_serverConfig :: ServerConfig,
  mas_users:: TVar [User],
  mas_messages:: TVar [API.Message]
}

type MyContext = Maybe JWTClaimsSet

type MyHandler o = ActionCtxT MyContext (WebStateM () MySession MyAppState) o

initAppState :: ServerConfig -> IO MyAppState
initAppState sc = do
  salt <- newSalt
  atomically $ do
    let user0 = createUser salt (API.CreateUserReq "admin@test.com" "xyzzy" True)
    users <- newTVar [user0]
    messages <- newTVar []
    return (MyAppState sc users messages)

instance HasJwtSecret MyAppState where
  getJwtSecret = sc_jwtSecret . mas_serverConfig

serverApp :: SpockM () MySession MyAppState ()
serverApp = do
  let api = API.mkApi
  adlPost (API.api_ping api) handlePing
  adlPost (API.api_login api) handleLogin
  adlPost (API.api_newMessage api) handleNewMessage
  adlPost (API.api_recentMessages api) handleRecentMessages
  adlPost (API.api_createUser api) handleCreateUser

handlePing :: Empty -> MyHandler Empty
handlePing _ = return Empty

handleLogin :: API.LoginReq -> MyHandler API.LoginResp
handleLogin API.LoginReq{API.lr_email, API.lr_password} = do
  st <- getState
  muser <- liftIO $ atomically $ do
    users <- readTVar $ mas_users st
    return (find (\u -> u_email u == lr_email) users)
  case muser of
    Nothing -> do
      return API.LR_failure
      error401 "bad login"
    (Just user) -> do
      case checkPass (mkPass lr_password) (PassHash (u_hashedPassword user)) of
        PassCheckFail -> error401 "bad login"
        PassCheckSuccess -> return ()
      let jwtSecret = sc_jwtSecret (mas_serverConfig st)
          header = mempty
          claims = mempty {
            unregisteredClaims = ClaimsMap (M.fromList [
              ("email", (JS.String (u_email user))),
              ("admin", (JS.Bool (u_isAdmin user)))
            ])
          }
          jwt = encodeSigned (hmacSecret jwtSecret) header claims
      return (API.LR_success jwt)

handleNewMessage :: API.NewMessageReq -> MyHandler Empty
handleNewMessage req = do
  now <- liftIO $ getCurrentTime
  ctx <- getContext
  st <- getState
  liftIO $ atomically $ do
    id <- (T.pack . show . (1+) . length) <$> readTVar (mas_messages st)
    let message = API.Message{
      API.m_id = id,
      API.m_body = API.nmr_body req,
      API.m_postedBy = maybe "??" emailFromClaims ctx,
      API.m_postedAt = TimeStamp now
    }
    modifyTVar' (mas_messages st) (message:)
  return Empty

handleRecentMessages :: API.RecentMessagesReq -> MyHandler [API.Message]
handleRecentMessages req = do
  st <- getState
  liftIO $ atomically $ do
    messages <- readTVar (mas_messages st)
    return (take ((fromIntegral (API.rmr_maxMessages req))) messages)

handleCreateUser :: API.CreateUserReq -> MyHandler API.CreateUserResp
handleCreateUser req = do
  st <- getState
  salt <- newSalt
  liftIO $ atomically $ do
    users <- readTVar $ mas_users st
    case find (\u -> u_email u == API.cur_email req) users of
      (Just _) ->
        return API.CUR_duplicateEmail
      Nothing -> do
        let user = createUser salt req
        let userid = (T.pack (show (length users + 1)))
        modifyTVar' (mas_users st) (user:)
        return (API.CUR_success (UserId userid))

createUser :: Salt -> API.CreateUserReq -> User
createUser salt req = User (API.cur_email req) (unPassHash hp) (API.cur_isAdmin req)
  where
    hp = hashPassWithSalt salt (mkPass (API.cur_password req))
