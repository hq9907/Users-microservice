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
import Data.String (IsString (fromString))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Class
import Database.Persist.Class.PersistEntity
import Database.Persist.Sql
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import GHC.Generics

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    Id Int Primary Unique default=generate_user_id() sql=user_id
    firstName Text
    lastName Text
    deriving Show Generic Eq
|]

data Link = Link {rel :: Text, href :: Text}
  deriving (Show, Generic, Eq)

data UserRspData = UserRspData Int User
  deriving (Show, Eq)

data UserRsp = UserRsp UserRspData [Link]
  deriving (Show, Eq)

data UsersRsp = UsersRsp [UserRsp] [Link]
  deriving (Show, Eq)

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
  toJSON (UserRspData userId (User firstName lastName)) =
    object
      [ "user_id" .= userId,
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
toUserRsp (Entity key user) = UserRsp (UserRspData userId user) links
  where
    links =
      [ Link "email" $ contactRoute "email",
        Link "phone" $ contactRoute "phone",
        Link "address" $ contactRoute "address",
        Link "carts" cartRoute
      ]
    contactRoute x = fromString $ "/contact/" ++ show userId ++ "/" ++ x
    cartRoute = fromString $ "/carts/" ++ show userId
    userId = unUserKey key

toUsersRsp :: [Entity User] -> Int -> Int -> Int -> UsersRsp
toUsersRsp entities total limit offset = UsersRsp (map toUserRsp entities) links
  where
    links =
      [ Link "prev" $ relativeRoute (-1),
        Link "curr" $ relativeRoute 0,
        Link "next" $ relativeRoute 1
      ]
    relativeRoute = relativePageRoute total limit offset

relativePageRoute :: Int -> Int -> Int -> Int -> Text
relativePageRoute total limit offset page = pageRoute $ offset + (page * limit)
  where
    pageRoute newOffset
      | newOffset < 0 = ""
      | newOffset >= total = ""
      | otherwise =
          fromString $
            "/users/?limit=" ++ show limit ++ "&offset=" ++ show newOffset
