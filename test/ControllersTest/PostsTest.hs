{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.PostsTest where

import Controllers.Posts
import ControllersTest.Data
import FromRequest

import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec

requestComments = defaultRequest {pathInfo = ["","","1"]}
  
postsTest :: IO ()
postsTest = hspec $ do
  describe "Testing Controllers.PostsTest" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb defaultRequest testRespond
          `shouldBe` return status401
    describe "Trying to get a posts" $ do
      it "Should fail if  if no post found" $ do
        let hTestDb' = hTestDb  {SD.findAllPosts = \req limit -> return []} 
        routes hTestLogger hTestTokenFalse hTestDb' defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found posts" $
        routes hTestLogger hTestTokenFalse hTestDb defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to get a comments" $ do
      it "Should fail if no comments found" $ do
        let hTestDb' = hTestDb  {SD.findComments = \req limit id_post -> return []}
        routes hTestLogger hTestTokenFalse hTestDb' requestComments testRespond
          `shouldBe` return status404
      it "Should successfully found comments" $
        routes hTestLogger hTestTokenFalse hTestDb requestComments testRespond
          `shouldBe` return status200
