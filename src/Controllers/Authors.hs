{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors
  ( routes
  ) where

import Data.Aeson (eitherDecode)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Monad (when)
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Author
import Services.Db
import Services.Logger
import Services.Token
import Utils

routes ::
     Pool Connection
  -> Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> respondWithError hLogger respond status400 "  Invalid or outdated token"
    Just (_, False) -> 
      respondWithError hLogger respond status400 "  Administrator authority required"
    Just (_, True) -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post
        "GET" -> get
        "DELETE" -> delete
        "PUT" -> put
        _ -> respondWithError hLogger respond status404 "  Invalid method"
    -- author creation (see example)
  where
    post = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String RawAuthor of
        Left e -> 
          respondWithError hLogger respond status400 ("  invalid request body  - " ++ e)
        Right correctlyParsedBody -> do
          id_ <- insertAuthor hDb pool correctlyParsedBody
          case id_ of
            0 -> 
              respondWithError hLogger respond status500 
                "  There is no user with this Id, or the user is already the author"
            _ -> respondWithSuccess respond status201 ("" :: String)
    -- show author, like
    -- http://localhost:3000/author/<token>/<id''>
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id''"
      authorMb <- findAuthorByID hDb pool id_
      case authorMb of
        Nothing -> respondWithError hLogger respond status404 "  RawAuthor not exist" 
        Just author -> respondWithSuccess respond status200 author
    delete = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id"
      deleteByID hDb pool "author" id_
      respondWithSuccess respond status204 ("" :: String)
    -- author editing (see example)
    put = do
      let id_ = toId req
          descrMb = toParam req "description"
      when (id_ == 0) $ do logError hLogger "  Invalid id"
      case descrMb of
        Nothing -> respondWithError hLogger respond status400 
                     "  The \"description\" parameter is required"
        Just descr -> do
          updateByID hDb pool "author" id_ descr
          respondWithSuccess respond status200 (RawAuthor id_ descr)
