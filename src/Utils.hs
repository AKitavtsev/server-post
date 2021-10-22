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

respondWithSuccess :: (ToJSON a, Monad m) =>
     (Response -> m b)
  -> Status
  -> a
  -> m b  
respondWithSuccess respond status context =
  respond (responseLBS status [("Content-Type", "text/plain")] $ encode context)
  
respondWithError :: Monad m =>
     Services.Logger.Handle m 
  -> (Response -> m b)
  -> Status
  -> String
  -> m b  
respondWithError hLogger respond status message= do
  logError hLogger message
  respond (responseLBS status [("Content-Type", "text/plain")] "")

respondWithImage :: Monad m =>
     (Response -> m b)
  -> (String, String)
  -> m b
respondWithImage respond (image, typeImage) =
  respond (responseLBS status200 [("Content-Type", BC.pack ("image/" ++ typeImage))] $
             BL.pack $ BC.unpack $ B64.decodeLenient $ BC.pack image)

respondWithPhotoId :: (Response -> m b) -> Integer -> m b
respondWithPhotoId respond photoId =
  case photoId of
    0 -> respond (responseLBS status201 [("Content-Type", "text/plain")] "")
    _ -> respond
          (responseLBS status201 [("Content-Type", "text/plain")] $
           BL.pack ("{photo_id:" ++ show photoId ++ "}"))

