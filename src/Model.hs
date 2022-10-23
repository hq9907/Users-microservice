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

data UserRspData = UserRspData (Key User) User
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
    links =
      [ Link "email" $ contactRoute "email",
        Link "address" $ contactRoute "address",
        Link "phone" $ contactRoute "phone",
        Link "carts" cartRoute
      ]
    contactRoute x = fromString $ "/contact/" ++ show key ++ "/" ++ x
    cartRoute = fromString $ "/carts/" ++ show key

toUsersRsp :: [Entity User] -> Int -> Int -> Int -> UsersRsp
toUsersRsp entities total limit offset = UsersRsp (map toUserRsp entities) links
  where
    links =
      [ Link "prev" $ relativeRoute (-1),
        Link "curr" $ relativeRoute 0,
        Link "next" $ relativeRoute 1
      ]
    relativeRoute x = pageRoute total limit $ offset + (x * limit)

pageRoute :: Int -> Int -> Int -> Text
pageRoute total limit offset
  | offset < 0 = ""
  | offset >= total = ""
  | otherwise =
      fromString $
        "/users/?limit=" ++ show limit ++ "&offset=" ++ show offset
