{-# LANGUAGE OverloadedStrings #-}

module Controllers.Photos where

import FromRequest

import Models.Draft (Photo(..))
import Services.Db
import Services.Logger
import Services.Token
import Utils

import Control.Exception
import Control.Monad (when)
import Data.Aeson
import Data.List (dropWhile)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

routes ::
     Pool Connection
  -> Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  case toMethod req of
    "POST" -> post
    "GET" -> get
    "PUT" -> put
    _ -> respondWithError hLogger respond status404 "  Invalid method"
  where
-- show photo, like
-- http://localhost:3000/photo/1
    get = do
      let id_ = toIdImage req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      imageMb <- findPhotoByID hDb pool id_
      case imageMb of
        Nothing -> respondWithError hLogger respond status400 "  Photo not found"
        Just imageAndType -> respondWithImage respond imageAndType
    post = do
      vt <- validToken hToken (toToken req)
      case vt of
        Nothing -> respondWithError hLogger respond status400 "  Invalid or outdated token"
        _ -> getPhoto >>= insertPhotoToDB >>= respondWithPhotoId respond
      where
        getPhoto = do
          body <- strictRequestBody req
          logDebug hLogger ("  Body = " ++ BL.unpack body)
          case eitherDecode body :: Either String Photo of
            Right correctlyParsedBody -> return correctlyParsedBody
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)
              return (Photo "" "")
-- uploading a photo to DB from a catalog file Images of the project root directory, like
-- PUT http://localhost:3000/photo/1.120210901202553ff034f3847c1d22f091dde7cde045264/?file=star.gif
    put = do
      vt <- validToken hToken (toToken req)
      case vt of
        Nothing -> respondWithError hLogger respond status400 "  Invalid or outdated token"
        _ -> insertPhotoFromFile >>= respondWithPhotoId respond
      where
        insertPhotoFromFile =
          verifiedParam (Photo "" "") >>= readPhotoFromFile >>=
          verifiedTypePhoto >>= insertPhotoToDB
        verifiedParam photo = do
          case toParam req "file" of
            Nothing -> do
              logError hLogger "  the \"file\" parameter is required"
              return photo
            Just fn -> return (Photo "" fn)
        readPhotoFromFile (Photo _ "") = return (Photo "" "")
        readPhotoFromFile (Photo _ fn) = do
          photoFile <-
            catch
              (BC.readFile ("Images/" ++ fn))
              (\e -> do
                 let err = show (e :: IOException)
                 logError hLogger ("  " ++ err)
                 return "")
          return (Photo (BC.unpack $ B64.encode photoFile) fn)
        verifiedTypePhoto (Photo _ "") = return (Photo "" "")
        verifiedTypePhoto (Photo photo fn) = do
          let dropFn = dropWhile (/= '.') fn
          case dropFn of
            "" -> do
              logError hLogger "  File type is not defined"
              return (Photo photo fn)
            _ -> return (Photo photo (tail dropFn))
    insertPhotoToDB :: Photo -> IO Integer
    insertPhotoToDB (Photo photo typ_) =
      if (photo == "") || (typ_ == "")
        then return 0
        else do
          id_ <- insertPhoto hDb pool (Photo photo typ_)
          when (id_ == 0) $
            logError
              hLogger
              (" Photo are of an invalid type" ++
               " (only png, jpg, gif or bmp is allowed).")
          return id_
