{-# LANGUAGE OverloadedStrings #-}

module  FromRequestTest where

import FromRequest

import Network.Wai
import Test.Hspec

         
req = defaultRequest 
       {queryString = [ ("tags_in",Just "[1,2]")
                      , ("title",Just "Paul M")]
       , pathInfo =   ["posts"
                      ,"1.120210901202553ff034f3847c1d22f091dde7cde045264"
                      , "1"]
       }
reqWithoutPathInfo = req { pathInfo = []}
reqWithoutIDs = req { pathInfo = ["posts" ,"1.1202", "x"]}

fromRequestTest :: IO ()
fromRequestTest = hspec $ do
    describe "Testing FromRequest" $ do
      describe "Trying to define Method from a Request" $ do
        it "Should successfully defined by default Method" $
          toMethod req `shouldBe` "GET"
        it "Should successfully defined Method " $ do
          let req' = req {requestMethod = "POST"}
          toMethod req' `shouldBe` "POST"
      describe "Trying to define request parameters" $ do
        it "Should successfully defined parameter" $
          toParam req "tags_in"  `shouldBe` Just "[1,2]"
        it "Should fail if parameter not found" $
          toParam req "tags" `shouldBe` Nothing
        it "Should fail if there are no parameters in the request" $ do 
          let reg' = req {queryString = []}
          toParam req "tag_i"  `shouldBe` Nothing
      describe "Trying to define path from a Request" $ do
        it "Should successfully defined path " $
          toPath req `shouldBe` "posts"
        it "Should fail if there is no first element in pathInfo" $
          toPath reqWithoutPathInfo `shouldBe` ""
      describe "Trying to define token from a Request" $ do
        it "Should successfully defined token" $
          toToken req `shouldBe`
                  "1.120210901202553ff034f3847c1d22f091dde7cde045264"
        it "Should fail if there is no second element in pathInfo" $
          toToken reqWithoutPathInfo `shouldBe` ""
      describe "Trying to define ID from a Request" $ do
        it "Should successfully defined ID" $
          toId req `shouldBe` 1
        it "Should fail if there is no third element in pathInfo" $
          toId  reqWithoutPathInfo `shouldBe` 0
        it "Should fail if the third element of the pathInfo not parsed into an Integer" $
          toIdImage reqWithoutIDs `shouldBe` 0
      describe "Trying to define ID of image from a Request" $ do
        it "Should successfully defined ID of image" $ do
          let req'  = req { pathInfo = ["image", "1"]}        
          toIdImage req' `shouldBe` 1 
        it "Should fail if there is no second element in pathInfo" $
          toIdImage reqWithoutPathInfo `shouldBe` 0
        it "Should fail if the second element of the pathInfo not parsed into an Integer" $ 
          toIdImage reqWithoutIDs `shouldBe` 0
 
