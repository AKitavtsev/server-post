{-# LANGUAGE OverloadedStrings #-}

module  TokenTest where

import Test.Hspec

import Services.Impl.MD5.Internal


tokenMD5Test :: IO ()
tokenMD5Test = hspec $ do
    describe "Services.Impl.MD5.Internal" $ do
      describe "idAdmFromToken" $ do
        it "is admin" $ 
          idAdmFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` Just ("1", "1")
        it "no admin" $ 
          idAdmFromToken "1.0202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` Just ("1", "0")
        it "all digit - no poin" $ 
          idAdmFromToken "102021043" `shouldBe` Just ("102021043", "0")
        it "[]" $ 
          idAdmFromToken "" `shouldBe` Nothing
      describe "timeFromToken" $ do
        it "time" $
          timeFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` "20210430162740"
        it "all digit - no poin" $ do
          timeFromToken "11202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` ""
        
