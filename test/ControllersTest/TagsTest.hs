{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.TagsTest where


import Controllers.Tags
import ControllersTest.Data
import FromRequest

import qualified Services.Token as ST
import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec


hTestRequestTag =
  hTestRequest {toBody = \req -> return "{\"tag\":\"Music\"}"}

tagsTest :: IO ()
tagsTest = hspec $ do
  describe "Testing Controllers.Tags" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status401
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqHEAD testRespond
          `shouldBe` return status404
    describe "Trying to add a tag" $ do
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status401
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status400
      it "Should successfully addition tag" $ do
        routes hTestLogger hTestTokenTrue hTestDb hTestRequestTag reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to get an tag" $ do 
      it "Should fail if author not found" $ do
        let hTestDb' = hTestDb  {SD.findTagByID = \id_ -> return Nothing} 
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequest defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found tag" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to delete a tag" $ do
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status401
      it "Should be success - if the tag was found, then deleted" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status204
    describe "Trying to update an author" $ do
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status401
      it "Should be success - if the tag was found, then updated" $ do
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status200
