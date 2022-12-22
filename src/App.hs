{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module App
  ( startApp,
    app,
  )
where

import DB (doMigration, runDB)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Persist
import Model
  ( Key (UserKey),
    Profile,
    User,
    UserRsp,
    UsersRsp,
    googleIdFilter,
    profileUpdate,
    toUserRsp,
    toUsersRsp,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Options
import Servant
import System.Environment (getArgs)

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app =
  cors (const $ Just policy) $
    provideOptions apiProxy $
      logStdoutDev $
        serve apiProxy server
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "POST", "PUT", "DELETE"],
          corsRequestHeaders = ["Authorization", "Content-Type"]
        }

startApp :: IO ()
startApp = do
  args <- getArgs
  let arg1 = if not (null args) then Just (head args) else Nothing
  case arg1 of
    Just "migrate" -> doMigration
    _ -> run 8080 app

type Api =
  "users"
    :> ( QueryParam "google_id" Text
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] UsersRsp
           :<|> Capture "id" Int :> Get '[JSON] UserRsp
           :<|> Capture "id" Int :> Delete '[JSON] ()
           :<|> Capture "id" Int :> ReqBody '[JSON] Profile :> Put '[JSON] ()
           :<|> ReqBody '[JSON] User :> Post '[JSON] UserRsp
       )

server :: Server Api
server =
  userGET
    :<|> userGETById
    :<|> userDELETE
    :<|> userPUT
    :<|> userPOST
  where
    userGET = selectUsers
    userGETById = selectUserById
    userDELETE = deleteUser
    userPUT = updateUser
    userPOST = createUser

selectUsers :: Maybe Text -> Maybe Int -> Maybe Int -> Handler UsersRsp
selectUsers mgid mlimit moffset = do
  userList <- runDB $ selectList selector [LimitTo limit, OffsetBy offset]
  total <- runDB $ count (selector :: [Filter User])
  return $ toUsersRsp userList total limit offset
  where
    limit = fromMaybe 20 mlimit
    offset = fromMaybe 0 moffset
    selector = googleIdFilter mgid

selectUserById :: Int -> Handler UserRsp
selectUserById userId = do
  sqlResult <- runDB $ getEntity $ UserKey userId
  case sqlResult of
    Just e -> return $ toUserRsp e
    Nothing -> throwError err404 {errBody = "User with ID not found."}

createUser :: User -> Handler UserRsp
createUser user = do
  e <- runDB $ insertEntity user
  return $ toUserRsp e

updateUser :: Int -> Profile -> Handler ()
updateUser userId profile = do runDB $ update (UserKey userId) $ profileUpdate profile

deleteUser :: Int -> Handler ()
deleteUser userId = do runDB $ delete $ UserKey userId
