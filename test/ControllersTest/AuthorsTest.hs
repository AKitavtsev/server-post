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


reqPOST = defaultRequest {requestMethod = "POST"}
reqHEAD = defaultRequest {requestMethod = "HEAD"}
reqDELETE = defaultRequest {requestMethod = "DELETE"}
reqPUT = defaultRequest {requestMethod = "PUT"}

hTestTokenFalse = hTestToken {ST.validToken = \token -> return (Just (1, False))}
hTestTokenTrue = hTestToken {ST.validToken = \token -> return (Just (1, True))}

hTestRequstRawAuthor = 
  hTestRequst {toBody = \req -> return "{\"author_id\":1, \"description\":\"\"}"}

authorsTest :: IO ()
authorsTest = hspec $ do
  describe "Testing Controllers.Authors" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequst defaultRequest testRespond
          `shouldBe` return status401
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequst defaultRequest testRespond
          `shouldBe` return status401
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequst reqHEAD testRespond
          `shouldBe` return status404
    describe "Trying to add an author" $ do
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequst reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if the user is not found, or the user is already the author" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequstRawAuthor reqPOST testRespond
          `shouldBe` return status500
      it "Should be success" $ do
        let hTestDb' = hTestDb  {SD.insertAuthor = \ra -> return 1}    
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequstRawAuthor reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to get an author" $ do 
      it "Should fail if author not found" $ do
        let hTestDb' = hTestDb  {SD.findAuthorByID = \ id_ -> return Nothing} 
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequst defaultRequest testRespond
          `shouldBe` return status404
      it "Should be success" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequst defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to delete an author" $ do
      it "Should be success - if the author was deleted" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequst reqDELETE testRespond
          `shouldBe` return status204
    describe "Trying to update an author" $ do
      it "Should fail if the request contains no data" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequst reqPUT testRespond
          `shouldBe` return status404 
      it "Should be success - if the author was, then updated" $ do
        let req = defaultRequest { requestMethod = "PUT"
                                 , queryString = [("description", Just "")]
                                 , pathInfo = ["","","1"]
                                 }
        routes hTestLogger hTestTokenTrue hTestDb hTestRequst req testRespond
          `shouldBe` return status200




