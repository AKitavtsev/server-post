{-# LANGUAGE OverloadedStrings #-}

module Controllers.Photos where

import FromRequest

import Models.Draft (Photo(..))
import Services.Db
import Services.Logger
import Services.Token
import Utils

import Control.Monad (when)
import Data.Aeson
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Lazy.Char8 as BL

routes :: Monad m =>
     Services.Logger.Handle m
  -> Services.Token.Handle m
  -> Services.Db.Handle m
  -> FromRequest.HandleRequest m
  -> Request
  -> (Response -> m b)
  -> m b
routes hLogger hToken hDb hRequest req respond = do
  case toMethod req of
    "POST" -> post
    "GET" -> get
    -- "PUT" -> put
    _ -> respondWithError hLogger respond status404 "  Invalid method"
-- show photo, like
-- http://localhost:3000/photo/1
  where
    get = do
      let id_ = toIdImage req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      imageMb <- findPhotoByID hDb id_
      case imageMb of
        Nothing ->
          respondWithError hLogger respond status400 "  Photo not found"
        Just imageAndType -> respondWithImage respond imageAndType
    post = do
      vt <- validToken hToken (toToken req)
      case vt of
        Nothing ->
          respondWithError
            hLogger
            respond
            status400
            "  Invalid or outdated token"
        _ -> getPhoto >>= insertPhotoToDB >>= respondWithPhotoId respond
      where
        getPhoto = do
          body <- toBody hRequest req
          logDebug hLogger ("  Body = " ++ BL.unpack body)
          case eitherDecode body :: Either String Photo of
            Right correctlyParsedBody -> return correctlyParsedBody
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)
              return (Photo "" "")
        insertPhotoToDB (Photo photo typ_) =
          if (photo == "") || (typ_ == "")
          then return 0
          else do
            id_ <- insertPhoto hDb (Photo photo typ_)
            when (id_ == 0) $
              logError
                hLogger
                (" Photo are of an invalid type" ++
                 " (only png, jpg, gif or bmp is allowed).")
            return id_
