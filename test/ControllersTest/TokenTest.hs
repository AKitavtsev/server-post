{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.TokenTest where


import Controllers.Token
import ControllersTest.Data
import FromRequest

import qualified Services.Token as ST
import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec

req = defaultRequest {queryString = [("login", Just ""), ("password", Just "")]}

tokenTest :: IO ()
tokenTest = hspec $ do
  describe "Testing Controllers.Token" $ do
    describe "Trying to create an token" $ do
      it "Should fail if the request does not contain a Login and Password" $
        routes hTestLogger hTestToken hTestDb defaultRequest testRespond
          `shouldBe` return status400
      it "Should fail if Invalid Login/Password" $ do
        let hTestDb' = hTestDb {SD.findUserByLogin = \login password -> return Nothing}      
        routes hTestLogger hTestToken hTestDb' req testRespond
          `shouldBe` return status404
      it "Should successfully created token" $ 
        routes hTestLogger hTestToken hTestDb req testRespond
          `shouldBe` return status201
