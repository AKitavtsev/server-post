{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  TokenTest where

import Test.Hspec

import Services.Impl.MD5.Internal


tokenTest :: IO ()
tokenTest = hspec $ do
    describe "Services.Impl.MD5.Internal" $ do
      describe "idAdmFromToken - take user from the token" $ do
        it "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222 - id = 1 (admin)" $ 
          idAdmFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` (Just ("1", "1"))
        it "1.0202104301627401b4056772f899bfcb6a0c827a3ccc222 - id = 1 (no admin)" $ 
          idAdmFromToken "1.0202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` (Just ("1", "0"))
        it "102021043 - id - 102021043(no admin)" $ 
          idAdmFromToken "102021043" `shouldBe` Just ("102021043", "0")
        it "\"\" - no token" $ 
          idAdmFromToken "" `shouldBe` Nothing
      describe "timeFromToken - take out the validity period of the token" $ do
        it "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222 - 2021-04-30 16:27:40" $
          timeFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` "20210430162740"
        it "11202104301627401b4056772f899bfcb6a0c827a3ccc222 - no date" $ do
          timeFromToken "11202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` ""
        
