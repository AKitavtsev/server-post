{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images
    where

import FromRequest
-- import Controllers.Token
import Servises.Db
import Servises.Logger
import Servises.Token
import Servises.Config


import Control.Monad.Trans
-- import Database.PostgreSQL.Simple
-- import Data.Pool (Pool)
-- import Network.HTTP.Types.Status
import Data.Aeson
-- import Data.Char (isDigit)
-- import Data.Hash.MD5
-- import Data.Pool (Pool)
-- import Data.Time.Clock
import Data.List (dropWhile)
import Control.Monad (when)
import GHC.Generics
import Network.HTTP.Types.Status
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64


routes pool hLogger hToken hDb req respond = do
  let token = toToken req
  vt <- validToken hToken token
  case  vt of
    Nothing -> do
       logError hLogger "  Invalid or outdated token"
       respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just _ -> do
      let id = toId req
      when (id == 0) $ do
        logError hLogger "  Invalid id"
      imageMb <- liftIO $ findImageByID hDb pool id
      case imageMb of
        Nothing -> do
          logError hLogger "  Image not found"
-- чисто для служебного пользования, для проекта надо оставить только
          let file = toParam req "file"
          case file of
            Nothing   -> respond (responseLBS notFound404 
                                  [("Content-Type", "text/plain")]
                                  "image not exist")
            Just fn -> do
              imageFile <- BC.readFile ("Images/" ++ fn) 
              let image = BC.unpack $ B64.encode imageFile              
              insertImage' hDb pool id image (tail $ dropWhile (\x -> not (x == '.')) fn)         
              respond (responseLBS notFound404 
                       [("Content-Type", "text/plain")]
                         "image adding image")    
--------------------
        Just (image, typ)  -> do
          let typeImage = BC.pack ("image/" ++ typ)    
          respond (responseLBS status200 [("Content-Type", typeImage)]
              $ BL.pack $ BC.unpack $ B64.decodeLenient $ BC.pack image)

  

