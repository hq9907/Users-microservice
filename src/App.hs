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
import Database.Persist
import Database.Persist.Class
import Model
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant
import System.Environment (getArgs)

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = simpleCors $ serve apiProxy server

startApp :: IO ()
startApp = do
  args <- getArgs
  let arg1 = if not (null args) then Just (head args) else Nothing
  case arg1 of
    Just "migrate" -> doMigration
    _ -> run 8080 app

type Api =
  "users"
    :> ( QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] UsersRsp
           :<|> Capture "id" Int :> Get '[JSON] UserRsp
           :<|> Capture "id" Int :> Delete '[JSON] ()
           :<|> Capture "id" Int :> ReqBody '[JSON] User :> Put '[JSON] ()
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

selectUsers :: Maybe Int -> Maybe Int -> Handler UsersRsp
selectUsers mlimit moffset = do
  userList <- runDB $ selectList [] [LimitTo limit, OffsetBy offset]
  total <- runDB $ count ([] :: [Filter User])
  return $ toUsersRsp userList total limit offset
  where
    limit = fromMaybe 20 mlimit
    offset = fromMaybe 0 moffset

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

updateUser :: Int -> User -> Handler ()
updateUser userId user = do runDB $ replace (UserKey userId) user

deleteUser :: Int -> Handler ()
deleteUser userId = do runDB $ delete $ UserKey userId
