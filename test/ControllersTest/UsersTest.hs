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


reqPOST = defaultRequest {requestMethod = "POST"}
reqDELETE = defaultRequest {requestMethod = "DELETE"}
reqPUT = defaultRequest {requestMethod = "PUT"}

hTestTokenFalse = hTestToken {ST.validToken = \token -> return (Just (1, False))}
hTestTokenTrue = hTestToken {ST.validToken = \token -> return (Just (1, True))}

hTestRequstRawUser =
  hTestRequst {toBody = \req -> return
  "{\"name\":\"\", \"surname\":\"\",  \"avatar\": {\"image\" : \"\" , \"typ\" : \"\"},  \"login\":\"\", \"password\":\"\"}"}

usersTest :: IO ()
usersTest = hspec $ do
  describe "Testing Controllers.Users" $ do
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestToken hTestDb hTestRequst reqPUT testRespond
          `shouldBe` return status404
    describe "Trying to add an user" $ do
      it "Should fail if invalid request body" $
        routes hTestLogger hTestToken hTestDb hTestRequst reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if login is already taken" $
        routes hTestLogger hTestToken hTestDb hTestRequstRawUser reqPOST testRespond
          `shouldBe` return status409
      it "Should successfully addition user" $ do
        let hTestDb' = hTestDb  {SD.insertUser = \user creation_date -> return 1}    
        routes hTestLogger hTestToken hTestDb' hTestRequstRawUser reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to get an user" $ do 
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequst defaultRequest testRespond
          `shouldBe` return status401
      it "Should fail if user not found" $ do
        let hTestDb' = hTestDb  {SD.findUserByID = \ id_ -> return Nothing} 
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequst defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found user" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequst defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to delete an user" $ do
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequst reqDELETE testRespond
          `shouldBe` return status401
      it "Should be success - if the user was deleted" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequst reqDELETE testRespond
          `shouldBe` return status204




