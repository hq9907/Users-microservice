{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
  )
import Data.Text (Text)
import Database.Persist (Entity (Entity))
import Database.Persist.Class.PersistEntity (Key)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics
import Servant (linkSegments)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    Id Int Primary Unique default=generate_user_id() sql=user_id
    firstName Text
    lastName Text
    deriving Show Generic
|]

data Link = Link {rel :: Text, href :: Text}
  deriving (Show, Generic)

data UserRspData = UserRspData (Key User) User

data UserRsp = UserRsp UserRspData [Link]

data UsersRsp = UsersRsp [UserRsp] [Link]

instance ToJSON User where
  toJSON (User firstName lastName) =
    object
      [ "first_name" .= firstName,
        "last_name" .= lastName
      ]

instance FromJSON User where
  parseJSON = withObject "User" $ \obj -> do
    firstName <- obj .: "first_name"
    lastName <- obj .: "last_name"
    return $ User firstName lastName

instance ToJSON Link

instance ToJSON UserRspData where
  toJSON (UserRspData key (User firstName lastName)) =
    object
      [ "user_id" .= key,
        "first_name" .= firstName,
        "last_name" .= lastName
      ]

instance ToJSON UserRsp where
  toJSON (UserRsp userData links) =
    object
      [ "data" .= toJSON userData,
        "links" .= toJSON links
      ]

instance ToJSON UsersRsp where
  toJSON (UsersRsp userRsps links) =
    object
      [ "data" .= toJSON userRsps,
        "links" .= toJSON links
      ]

toUserRsp :: Entity User -> UserRsp
toUserRsp (Entity key user) = UserRsp (UserRspData key user) links
  where
    links = []

toUsersRsp :: [Entity User] -> UsersRsp
toUsersRsp entities = UsersRsp (map toUserRsp entities) links
  where
    links = []
