{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors
  ( routes
  ) where

import Data.Aeson (eitherDecode)

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

routes :: Monad m =>
     Services.Logger.Handle m
  -> Services.Token.Handle m
  -> Services.Db.Handle m
  -> FromRequest.HandleRequst m
  -> Request
  -> (Response -> m b)
  -> m b
routes hLogger hToken hDb hRequest req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing ->
      respondWithError hLogger respond status400 "  Invalid or outdated token"
    Just (_, False) ->
      respondWithError
        hLogger
        respond
        status400
        "  Administrator authority required"
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
      body <- toBody hRequest req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String RawAuthor of
        Left e ->
          respondWithError
            hLogger
            respond
            status400
            ("  invalid request body  - " ++ e)
        Right correctlyParsedBody -> do
          id_ <- insertAuthor hDb correctlyParsedBody
          case id_ of
            0 ->
              respondWithError
                hLogger
                respond
                status500
                "  There is no user with this Id, or the user is already the author"
            _ -> respondWithSuccess respond status201 ("" :: String)
    -- show author, like
    -- http://localhost:3000/author/<token>/<id''>
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id''"
      authorMb <- findAuthorByID hDb id_
      case authorMb of
        Nothing ->
          respondWithError hLogger respond status404 "  Author not exist"
        Just author -> respondWithSuccess respond status200 author
    delete = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id"
      deleteByID hDb "author" id_
      respondWithSuccess respond status204 ("" :: String)
    -- author editing (see example)
    put = do
      let id_ = toId req
          descrMb = toParam req "description"
      when (id_ == 0) $ do logError hLogger "  Invalid id"
      case descrMb of
        Nothing ->
          respondWithError
            hLogger
            respond
            status400
            "  The \"description\" parameter is required"
        Just descr -> do
          updateByID hDb "author" id_ descr
          respondWithSuccess respond status200 (RawAuthor id_ descr)
