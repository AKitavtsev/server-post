{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.ImagesTest where

import Controllers.Images
import ControllersTest.Data
import FromRequest

import qualified Services.Db as SD

import Network.Wai
import Network.HTTP.Types.Status
import Test.Hspec


imagesTest :: IO ()
imagesTest = hspec $ do
  describe "Testing Controllers.Images" $ do
    describe "Trying to get an image" $ do 
      it "Should fail if image not found" $ do
        let hTestDb' = hTestDb  {SD.findImageByID = \ id_ -> return Nothing} 
        routes hTestLogger hTestDb' defaultRequest testRespond
          `shouldBe` return status404
      it "Should successfully found image" $
        routes hTestLogger hTestDb defaultRequest testRespond
          `shouldBe` return status200
