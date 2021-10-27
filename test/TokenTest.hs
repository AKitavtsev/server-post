{-# LANGUAGE OverloadedStrings #-}

module  TokenTest where

import Test.Hspec

import Services.Impl.MD5.Internal


tokenMD5Test :: IO ()
tokenMD5Test = hspec $ do
    describe "Testing Services.Impl.MD5.Internal" $ do
      describe "Trying to determine by the token whether the user has admin status" $ do
        it "Should successfully determined that the user is admin" $ 
          idAdmFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` Just ("1", "1")
        it "Should successfully determined that the user is no admin" $ 
          idAdmFromToken "1.0202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` Just ("1", "0")
        it "It must be conditionally defined that the user is no admin (invalid token format)" $ 
          idAdmFromToken "102021043" `shouldBe` Just ("102021043", "0")
        it "Should fail if no token is specified" $ 
          idAdmFromToken "" `shouldBe` Nothing
      describe "Trying to get shelf life from token" $ do
        it "Should successfully get shelf life" $
          timeFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` "20210430162740"
        it "Should fail - invalid token format" $ do
          timeFromToken "11202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` ""
        
