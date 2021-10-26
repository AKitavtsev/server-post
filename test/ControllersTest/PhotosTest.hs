{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.PhotosTest where

import Controllers.Photos
import ControllersTest.Data
import FromRequest

import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec

hTestRequestPhoto =
  hTestRequest {toBody = \req -> return "{\"image\" : \"image\" , \"typ\" : \"jpg\"}"}
  
photosTest :: IO ()
photosTest = hspec $ do
  describe "Testing Controllers.PhotosTest" $ do
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status404
    describe "Trying to get a photo" $ do
      it "Should fail if photo not found" $ do
        let hTestDb' = hTestDb  {SD.findPhotoByID = \id_ -> return Nothing} 
        routes hTestLogger hTestTokenFalse hTestDb' hTestRequest defaultRequest testRespond
          `shouldBe` return status400
      it "Should successfully found photo" $
        routes hTestLogger hTestToken hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to add a photo" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status401
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if if the photo type is wrong" $        
        routes hTestLogger hTestTokenFalse hTestDb hTestRequestPhoto reqPOST testRespond
          `shouldBe` return status400
      it "Should successfully addition photo" $ do
        let hTestDb' = hTestDb  {SD.insertPhoto = \ph -> return 1}    
        routes hTestLogger hTestTokenFalse hTestDb' hTestRequestPhoto reqPOST testRespond
          `shouldBe` return status201
