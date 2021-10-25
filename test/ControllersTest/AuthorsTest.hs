{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.AuthorsTest where


import Controllers.Authors
import ControllersTest.Data
import FromRequest

import qualified Services.Token as ST
import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec


hTestRequestRawAuthor = 
  hTestRequest {toBody = \req -> return "{\"author_id\":1, \"description\":\"\"}"}

authorsTest :: IO ()
authorsTest = hspec $ do
  describe "Testing Controllers.Authors" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status401
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status401
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqHEAD testRespond
          `shouldBe` return status404
    describe "Trying to add an author" $ do
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if the user is not found, or the user is already the author" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequestRawAuthor reqPOST testRespond
          `shouldBe` return status500
      it "Should successfully addition author" $ do
        let hTestDb' = hTestDb  {SD.insertAuthor = \ra -> return 1}    
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequestRawAuthor reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to get an author" $ do 
      it "Should fail if author not found" $ do
        let hTestDb' = hTestDb  {SD.findAuthorByID = \ id_ -> return Nothing} 
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequest defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found author" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to delete an author" $ do
      it "Should be success - if the author was found, then deleted" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status204
    describe "Trying to update an author" $ do
      it "Should fail if the request contains no data" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status404 
      it "Should be success - if the author was found, then updated" $ do
        let req = defaultRequest { requestMethod = "PUT"
                                 , queryString = [("description", Just "")]
                                 , pathInfo = ["","","1"]
                                 }
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest req testRespond
          `shouldBe` return status200




