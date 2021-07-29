{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Photos
    where

import FromRequest
-- import Controllers.Token
import Servises.Db
import Servises.Logger
import Servises.Token
import Servises.Config
import Models.Draft (Photo (..))

import Control.Exception
import Control.Monad.Trans
import Data.Aeson
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
import qualified Data.Text as T

routes pool hLogger hToken hDb req respond = do
  case  toMethod req of
    "POST"   -> post        
    "GET"    -> get
    "PUT"    -> put
    _        -> do 
      logError hLogger "  Invalid method"
      respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
  where
  get = do
    let id = toIdImage req
    when (id == 0) $ do
      logError hLogger "  Invalid id"
    imageMb <- liftIO $ findPhotoByID hDb pool id
    case imageMb of
      Nothing -> do
        logError hLogger "  Photo not found"
        respond $ responseLBS status400 [("Content-Type", "text/plain")] ""         
      Just (image, typ)  -> do
        let typeImage = BC.pack ("image/" ++ typ)    
        respond (responseLBS status200 [("Content-Type", typeImage)]
              $ BL.pack $ BC.unpack $ B64.decodeLenient $ BC.pack image)
  post = do
    vt <- validToken hToken (toToken req)
    case  vt of
      Nothing -> do
        logError hLogger "  Invalid or outdated token"
        respond (responseLBS status400 [("Content-Type", "text/plain")] "")
      Just (id_author, _) -> do
        photo <- getPhoto
        idim  <- insertPhotoToDB photo 
        case idim of
          0 ->  respond (responseLBS status404 [("Content-Type", "text/plain")] "")
          _ ->  respond (responseLBS status201 [("Content-Type", "text/plain")] 
                            $ BL.pack ("{photo_id:" ++ (show idim) ++ "}"))                       
    where 
      getPhoto = do
        body <- strictRequestBody req
        logDebug hLogger ("  Body = " ++ (BL.unpack body))
        case eitherDecode body :: Either String Photo of
          Right correctlyParsedBody -> return correctlyParsedBody
          Left  e -> do
            logError hLogger ("  Invalid request body  - " ++ e)          
            return (Photo "" "")

  put = do
    vt <- validToken hToken (toToken req)
    case vt of
      Nothing -> do
        logError hLogger "  Invalid or outdated token"
        respond (responseLBS status400 [("Content-Type", "text/plain")] "")
      Just (id_author, _) -> do 
         idim <- insertPhotoFromFile
         case idim of 
           0  ->  respond (responseLBS status404 [("Content-Type", "text/plain")] "")
           _  ->  respond (responseLBS status201
                       [("Content-Type", "text/plain")]
                       $ BL.pack ("{photo_id:" ++ (show idim) ++ "}"))
    where
      insertPhotoFromFile = return (Photo "" "") >>=
                            verifiedParam        >>=
                            readPhotoFromFile    >>=
                            verifiedTypePhoto    >>=
                            insertPhotoToDB
      verifiedParam photo = do 
        case toParam req "file" of
          Nothing -> do
            logError hLogger ("  the \"file\" parameter is required")           
            return photo 
          Just fn -> return (Photo "" fn)
      readPhotoFromFile (Photo _ "") = return (Photo "" "")
      readPhotoFromFile (Photo _ fn) = do 
        photoFile <- catch (BC.readFile ("Images/" ++ fn))
                         (\e -> do
                              let err = show (e :: IOException)
                              logError hLogger ("  " ++ err)
                              return "")   
        return (Photo (BC.unpack $ B64.encode photoFile) fn) 
      verifiedTypePhoto (Photo _ "") = return (Photo "" "")
      verifiedTypePhoto (Photo photo fn) = do
        let dropFn = dropWhile (\x -> not (x == '.')) fn 
        case dropFn of
          "" -> do 
            logError hLogger ("  File type is not defined")
            return (Photo photo fn)
          _  -> return (Photo photo (tail dropFn))
  
  insertPhotoToDB :: Photo -> IO Integer
  insertPhotoToDB (Photo photo typ)  = 
    if ((photo == "") || (typ == ""))
    then return 0
    else do 
      id <- insertPhoto hDb pool (Photo photo typ)
      when (id == 0) $ do 
        logError hLogger 
          (" Photo are of an invalid type" ++
           " (only png, jpg, gif or bmp is allowed).") 
      return id                