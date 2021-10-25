{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.CommentTest where


import Controllers.Comments
import ControllersTest.Data
import FromRequest

import qualified Services.Token as ST
import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec


hTestRequestRawComment = 
  hTestRequest {toBody = \req -> return "{\"post_id\": 1, \"comment\":\"\"}"}

commentTest :: IO ()
commentTest = hspec $ do
  describe "Testing Controllers.Comment" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status401
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status404
    describe "Trying to add a comment" $ do
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if the post is not found" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequestRawComment reqPOST testRespond
          `shouldBe` return status404
      it "Should successfully addition comment" $ do
        let hTestDb' = hTestDb  {SD.insertComment = \rc auth_id creation_date -> return 1}
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequestRawComment reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to delete a comment" $ do
      it "Should fail if ID of comment not is specified" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqDELETE testRespond        
          `shouldBe` return status400
      it "Should be success - if the comment was found, then deleted" $ do
        let req = defaultRequest { requestMethod = "DELETE"
                                 , pathInfo = ["","","1"]
                                 }
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest req testRespond        
          `shouldBe` return status204
