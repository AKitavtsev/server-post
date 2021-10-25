{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.DraftsTest where


import Controllers.Drafts
import ControllersTest.Data
import FromRequest

import qualified Services.Token as ST
import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec


hTestRequestRawDraft = 
  hTestRequest {toBody = \req -> return "{\"title\":\"\", \"category\":1, \"tags\":[],  \"text_content\":\"\", \"main_photo\":1, \"other_photos\":[]}"}
hTestRequestForUpdateDraft = 
  hTestRequest {toBody = \req -> return"{\"id_draft\":1,\"new_title\":\"\",\"new_content\":\"\",\"new_tags\":[], \"new_main_photo\":4, \"new_category\":5,\"new_other_photos\":[]}"}

draftsTest :: IO ()
draftsTest = hspec $ do
  describe "Testing Controllers.Drafts" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status401
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqHEAD testRespond
          `shouldBe` return status404
    describe "Trying to add a draft" $ do
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if category not found" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequestRawDraft reqPOST testRespond
          `shouldBe` return status400
      it "Should successfully addition draft" $ do
        let hTestDb' = hTestDb  {SD.insertDraft = \rd id_ date -> return 1}    
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequestRawDraft reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to get a draft" $ do 
      it "Should fail if draft not found" $ do
        let hTestDb' = hTestDb  {SD.findDraftByID = \ id_ -> return Nothing} 
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequest defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found draft" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to delete a draft" $ do
      it "Should be success - if the draft was found, then deleted" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status204
    describe "Trying to update a draft" $ do
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status400
      it "Should fail if draft, category or main photo not found" $ do
        let hTestDb' = hTestDb  {SD.updateDraft = \fud auth -> return Nothing}  
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequestForUpdateDraft reqPUT testRespond
          `shouldBe` return status400
      it "Should be success - if the draft was found, then updated" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequestForUpdateDraft reqPUT testRespond
          `shouldBe` return status200
