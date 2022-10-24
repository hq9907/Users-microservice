{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ModelSpec (spec) where

import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types
import Database.Persist.Class.PersistEntity
import Model
import Test.Hspec

user :: User
user = User "first name" "last name"

userJSON :: Value
userJSON =
  [aesonQQ|
  {
    "first_name": "first name",
    "last_name": "last name"
  }
|]

userRsp :: UserRsp
userRsp =
  UserRsp
    (UserRspData 0 user)
    [ Link "email" "/contact/0/email",
      Link "phone" "/contact/0/phone",
      Link "address" "/contact/0/address",
      Link "carts" "/carts/0"
    ]

userRspJSON :: Value
userRspJSON =
  [aesonQQ|
  {
    "data": {
      "user_id": 0,
      "first_name": "first name",
      "last_name": "last name"
    },
    "links": [
      {
        "rel": "email",
        "href": "/contact/0/email"
      },
      {
        "rel": "phone",
        "href": "/contact/0/phone"
      },
      {
        "rel": "address",
        "href": "/contact/0/address"
      },
      {
        "rel": "carts",
        "href": "/carts/0"
      }
    ]
  }
|]

usersRsp :: UsersRsp
usersRsp =
  UsersRsp
    [userRsp]
    [ Link "prev" "",
      Link "curr" "/users/?limit=20&offset=0",
      Link "next" "/users/?limit=20&offset=20"
    ]

usersRspJSON :: Value
usersRspJSON =
  [aesonQQ|
  {
    "data": [#{userRsp}],
    "links": [
      {
        "rel": "prev",
        "href": ""
      },
      {
        "rel": "curr",
        "href": "/users/?limit=20&offset=0"
      },
      {
        "rel": "next",
        "href": "/users/?limit=20&offset=20"
      }
    ]
  }
|]

spec :: Spec
spec = do
  describe "User" $ do
    it "parse to JSON correctly" $ do
      toJSON user `shouldBe` userJSON
    it "parse from JSON correctly" $ do
      parseMaybe parseJSON userJSON `shouldBe` Just user

  describe "UserRsp" $ do
    it "parse to JSON correctly" $ do
      toJSON userRsp `shouldBe` userRspJSON
    it "correctly wrap User" $ do
      toUserRsp (Entity (UserKey 0) user) `shouldBe` userRsp

  describe "UsersRsp" $ do
    it "parse to JSON correctly" $ do
      toJSON usersRsp `shouldBe` usersRspJSON
    it "correctly wrap User array" $ do
      toUsersRsp [(Entity (UserKey 0) user)] 50 20 0 `shouldBe` usersRsp

  describe "Pagination" $ do
    it "return empty 'prev' if it's the first page" $ do
      relativePageRoute 20 20 0 (-1) `shouldBe` ""
    it "return empty 'next' if it's the last page" $ do
      relativePageRoute 40 20 20 1 `shouldBe` ""
    it "return correct 'prev' page route" $ do
      relativePageRoute 40 20 20 (-1) `shouldBe` "/users/?limit=20&offset=0"
    it "return correct 'curr' page route" $ do
      relativePageRoute 40 20 20 0 `shouldBe` "/users/?limit=20&offset=20"
    it "return correct 'next' page route" $ do
      relativePageRoute 40 20 0 1 `shouldBe` "/users/?limit=20&offset=20"
