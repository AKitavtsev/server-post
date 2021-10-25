{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.CategoriesTest where


import Controllers.Categories
import ControllersTest.Data
import FromRequest

import qualified Services.Token as ST
import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec


reqParent = defaultRequest { requestMethod = "PUT"
                           , queryString = [("name", Just ""),
                                            ("parent", Just "1")
                                           ]
                                 , pathInfo = ["","","1"]
                                 }
hTestRequestCategory = 
  hTestRequest {toBody = \req -> return "{\"name\":\"\", \"id_parent\": 1}"}

categoriesTest :: IO ()
categoriesTest = hspec $ do
  describe "Testing Controllers.Categories" $ do
    describe "Testing token" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status401
    describe "Testing request" $ do
      it "Should fail if invalid request method" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqHEAD testRespond
          `shouldBe` return status404
    describe "Trying to add a category" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status401
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status401
      it "Should fail if invalid request body" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqPOST testRespond
          `shouldBe` return status400
      it "Should fail if category with non-existent parent " $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequestCategory reqPOST testRespond
          `shouldBe` return status500
      it "Should successfully addition category" $ do
        let hTestDb' = hTestDb  {SD.insertCategory = \cat -> return 1}    
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequestCategory reqPOST testRespond
          `shouldBe` return status201
    describe "Trying to get a category" $ do 
      it "Should fail if category not found" $ do
        let hTestDb' = hTestDb  {SD.findCategoryByID = \ id_ -> return Nothing} 
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequest defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found category" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest defaultRequest testRespond
          `shouldBe` return status200
    describe "Trying to delete an author" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status401
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status401
      it "Should be success - if the category was found, then deleted" $
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqDELETE testRespond
          `shouldBe` return status204
    describe "Trying to update a category" $ do
      it "Should fail if invalid or outdated token" $
        routes hTestLogger hTestToken hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status401
      it "Should fail if there is no administrator rights" $
        routes hTestLogger hTestTokenFalse hTestDb hTestRequest reqPUT testRespond
          `shouldBe` return status401
    describe "Trying to update a name of category" $ do     
      it "Should be success - name has been changed, and the parent does not need to be changed " $ do
        let req = defaultRequest { requestMethod = "PUT"
                                 , queryString = [("name", Just "")]
                                 , pathInfo = ["","","1"]
                                 }           
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest req testRespond
          `shouldBe` return status200
    describe "Trying to update a parent of category" $ do     
      it "Should fail if if parent is not found" $ do
        routes hTestLogger hTestTokenTrue hTestDb hTestRequest reqParent testRespond
          `shouldBe` return status200
      it "Should successfully updated parent of the category" $ do
        let hTestDb' = hTestDb  {SD.updateParentCategory = \id_ parent -> return 1}
        routes hTestLogger hTestTokenTrue hTestDb' hTestRequest reqParent testRespond
          `shouldBe` return status200




