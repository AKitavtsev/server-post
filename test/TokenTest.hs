{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  TokenTest where

import Test.Hspec
import Network.HTTP.Types.Status


import Token


tokenTest :: IO ()

tokenTest = hspec $ do
    describe "Token" $ do
      describe "idAdmFromToken" $ do
        it "is admin" $ 
          idAdmFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` (Just (1, True))
        it "no admin" $ 
          idAdmFromToken "1.0202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` (Just (1, False))
        it "all digit - no poin" $ 
          idAdmFromToken "102021043" `shouldBe` Just (102021043, False)
        it "[]" $ 
          idAdmFromToken "" `shouldBe` Nothing
      describe "creatToken" $ do
        it "just token" $ 
          creatToken (Just (1, True))  "20210430162740"
          `shouldBe` (Just (Token "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"))
      describe "timeFromToken" $ do
        it "time" $
          timeFromToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222"
          `shouldBe` "20210430162740"
      describe "testToken" $ do
        it "OK" $
          testToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222" "20210401162740"
          `shouldBe` Right (1, True)
        it "old token" $
          testToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc222" "20210501162740"
          `shouldBe` Left (Status 403 "Old token")
        it "wrong token" $
          testToken "1.1202104301627401b4056772f899bfcb6a0c827a3ccc225" "20210301162740"
          `shouldBe` Left (Status 402 "Bad token")
        it "not token at all" $
          testToken "" ""
          `shouldBe` Left (Status 401 "Bad token")
        
