{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.UsersTest where


import Controllers.Users
import ControllersTest.Data
import FromRequest

import qualified Services.Token as ST
import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec


hTestRequestRawUser =
  hTestRequest {toBody = \req -> return
  "{\"name\":\"\", \"surname\":\"\",  \"avatar\": {\"image\" : \"\" , \"typ\" : \"\"},  \"login\":\"\", \"password\":\"\"}"}

usersTest :: IO ()
usersTest = hspec $ do
  describe "Testing Controllers.Users" $ do
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status404
    describe "Trying to add an user" $ do
      it "Should fail if invalid request body" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if login is already taken" $
        routes hTestLogger hTestToken hTestDb hTestRequestRawUser reqPOST testRespond
          `shouldBe` return status409
      it "Should successfully addition user" $ do
        let hTestDb' = hTestDb  {SD.insertUser = \user creation_date -> return 1}    
        routes hTestLogger hTestToken hTestDb' hTestRequestRawUser reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to get an user" $ do 
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status401
      it "Should fail if user not found" $ do
        let hTestDb' = hTestDb  {SD.findUserByID = \ id_ -> return Nothing} 
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequest defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found user" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to delete an user" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status401
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status401
      it "Should be success - if the user was found, then deleted" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status204




