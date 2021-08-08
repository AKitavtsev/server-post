{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  DbTest where

import Test.Hspec

import Servises.Impl.PostgreSQL.Internal

dbTest :: IO ()

dbTest = hspec $ do
    describe "Servises.Impl.PostgreSQL.Internal" $ do
      describe "fromPhotoId" $ do
        it "link to photo" $ 
          fromPhotoId 1 `shouldBe` "http://localhost:3000/photo/1"
      describe "toListString" $ do
        it "list of String" $ 
          toListString "{aaa, bb, c}" `shouldBe` ["aaa", "bb", "c"]
      describe "toListInteger" $ do
        it "list of Integer" $ 
          toListInteger "{1, 2, 3}" `shouldBe` [1, 2, 3]
      describe "read'" $ do
        it "is integer" $ 
          read' "123" `shouldBe` 123
        it "is wrong integer " $ 
          read' "12h" `shouldBe` 1
        
