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
import Database.Persist
  ( Entity (Entity),
    PersistStoreRead (get),
    PersistStoreWrite (delete, insert),
    selectList,
  )
import Model (Key (UserKey), User)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (getArgs)

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

startApp :: IO ()
startApp = do
  args <- getArgs
  let arg1 = if not (null args) then Just (head args) else Nothing
  case arg1 of
    Just "migrate" -> doMigration
    _ -> run 8080 app

type Api =
  "users"
    :> ( Get '[JSON] [User]
           :<|> Capture "id" Int :> Get '[JSON] User
           :<|> Capture "id" Int :> Delete '[JSON] ()
           :<|> ReqBody '[JSON] User :> Post '[JSON] User
       )

server :: Server Api
server =
  userGET
    :<|> userGETById
    :<|> userDELETE
    :<|> userPOST
  where
    userGET = selectUsers
    userGETById = selectUserById
    userDELETE = deleteUser
    userPOST = createUser

selectUsers :: Handler [User]
selectUsers = do
  userList <- runDB $ selectList [] []
  return $ map (\(Entity _ u) -> u) userList

selectUserById :: Int -> Handler User
selectUserById userId = do
  sqlResult <- runDB $ get $ UserKey userId
  case sqlResult of
    Just user -> return user
    Nothing -> throwError err404 {errBody = "User with ID not found."}

createUser :: User -> Handler User
createUser user = do
  _ <- runDB $ insert user
  return user

deleteUser :: Int -> Handler ()
deleteUser userId = do runDB $ delete $ UserKey userId
