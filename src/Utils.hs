{-# LANGUAGE OverloadedStrings #-}

module Utils
  where

import Data.Aeson 
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)

import Services.Logger

respondWithSuccess :: ToJSON a =>
     (Response -> IO b)
  -> Status
  -> a
  -> IO b  
respondWithSuccess respond status context =
  respond (responseLBS status [("Content-Type", "text/plain")] $ encode context)
  
respondWithError :: 
     Services.Logger.Handle   
  -> (Response -> IO b)
  -> Status
  -> String
  -> IO b  
respondWithError hLogger respond status message= do
  logError hLogger message
  respond (responseLBS status [("Content-Type", "text/plain")] "")

respondWithImage ::
     (Response -> IO b)
  -> (String, String)
  -> IO b
respondWithImage respond (image, typeImage) =
  respond (responseLBS status200 [("Content-Type", BC.pack ("image/" ++ typeImage))] $
             BL.pack $ BC.unpack $ B64.decodeLenient $ BC.pack image)

respondWithPhotoId :: (Response -> IO b) -> Integer -> IO b
respondWithPhotoId respond photoId =
  case photoId of
    0 -> respond (responseLBS status201 [("Content-Type", "text/plain")] "")
    _ -> respond
          (responseLBS status201 [("Content-Type", "text/plain")] $
           BL.pack ("{photo_id:" ++ show photoId ++ "}"))

