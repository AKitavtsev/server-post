{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.PublishTest where

import Controllers.Publish
import ControllersTest.Data
import FromRequest

import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec

  
publishTest :: IO ()
publishTest = hspec $ do
  describe "Testing Controllers.PublishTest" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb defaultRequest testRespond
          `shouldBe` return status401
    describe "Trying to get a posts" $ do
      it "Should fail if draft no found" $
        routes hTestLogger hTestTokenFalse hTestDb defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully draft published " $ do
        let hTestDb' = hTestDb  {SD.publishPost = \draft auth -> return 1}
        routes hTestLogger hTestTokenFalse hTestDb' defaultRequest testRespond
          `shouldBe` return status200
