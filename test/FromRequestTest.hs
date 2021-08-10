{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  FromRequestTest where

import FromRequest

import Network.Wai
import Test.Hspec

import qualified Data.Text as T
         
req = defaultRequest 
       { requestMethod = "GET"
       , queryString = [ ("tags_in",Just "[1,2]")
                       , ("title",Just "Paul M")
                       , ("created_gt",Just "2021-07-10")
                       , ("name",Just "Bred")
                       , ("category",Just "5")
                       , ("find",Just "Pau")
                       , ("text",Just "McC")]
       , pathInfo =    ["posts"
                        ,"1.120210901202553ff034f3847c1d22f091dde7cde045264"
                        , "1"]
       }
req'    = req { pathInfo = []}
req''   = req { pathInfo = ["posts" ,"1.1202", "x"]}
req'''  = req { pathInfo = ["image", "1"]}

fromRequestTest :: IO ()
fromRequestTest = hspec $ do
    describe "FromRequestTest" $ do
      describe "toMethod" $ do
        it "method" $ 
          toMethod req `shouldBe` "GET"
      describe "toParam" $ do
        it "is parameter" $
          toParam req "tags_in"  `shouldBe` Just "[1,2]"
        it "no parameter" $
          toParam req "tag_i"  `shouldBe` Nothing
      describe "toPath" $ do
        it "is path" $
          toPath req `shouldBe` "posts"
        it "no path" $
          toPath req' `shouldBe` ""
      describe "toToken" $ do
        it "is token" $
          toToken req `shouldBe`
                  "1.120210901202553ff034f3847c1d22f091dde7cde045264"
        it "no token" $
          toToken req' `shouldBe` ""
      describe "toId" $ do
        it "is ID" $
          toId req `shouldBe` 1
        it "no ID" $
          toId req' `shouldBe` 0
      describe "toIdImage" $ do
        it "is ID Image" $
          toIdImage req''' `shouldBe` 1
        it "no ID Image" $
          toIdImage req' `shouldBe` 0
        it "ID Image not parse" $
          toIdImage req `shouldBe` 0
 
