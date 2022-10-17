{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DB (runDB, connInfo, doMigration) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson as JSON (FromJSON, decode)
import Data.ByteString.Lazy (readFile)
import Database.Persist.MySQL
  ( ConnectInfo (..),
    defaultConnectInfo,
    runMigration,
    withMySQLConn,
  )
import Database.Persist.Sql (SqlPersistT, runSqlConn)
import GHC.Generics (Generic)
import Model (migrateAll)
import Servant (Handler)

data DBConfig = DBConfig
  { host :: String,
    user :: String,
    password :: String
  }
  deriving (Show, Generic)

instance FromJSON DBConfig

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> Handler a
runDB a = do
  conn <- liftIO connInfo
  liftIO $ runNoLoggingT $ runResourceT $ withMySQLConn conn $ runSqlConn a

connInfo :: IO ConnectInfo
connInfo = do
  configJSON <- Data.ByteString.Lazy.readFile "config.json"
  case JSON.decode configJSON of
    Just (DBConfig host user password) ->
      return
        defaultConnectInfo
          { connectHost = host,
            connectUser = user,
            connectPassword = password,
            connectDatabase = "Users"
          }
    _ -> error "Invalid config.json"

doMigration :: IO ()
doMigration = do
  conn <- connInfo
  runNoLoggingT $ runResourceT $ withMySQLConn conn $ runReaderT $ runMigration migrateAll
