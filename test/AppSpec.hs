{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AppSpec (spec) where

import App (app)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (get, pending, shouldRespondWith, with)

spec :: Spec
spec = with (return app) $ do
  describe "GET /users" $ do
    it "responds with 200" $ do
      pending

-- get "/users" `shouldRespondWith` 200
