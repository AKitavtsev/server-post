{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors
  ( routes
  ) where

import Data.Aeson (eitherDecode, encode)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Control.Monad (when)
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Author
import Services.Db
import Services.Logger
import Services.Token

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
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (_, False) -> do
      logError hLogger "  Administrator authority required"
      respond
        (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
    Just (_, True) -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post
        "GET" -> get
        "DELETE" -> delete
        "PUT" -> put
        _ -> do
          logError hLogger "  Invalid method"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
    -- author creation (see example)
  where
    post = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String RawAuthor of
        Left e -> do
          logError hLogger ("  Invalid request body  - " ++ e)
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Right correctlyParsedBody -> do
          id_ <- insertAuthor hDb pool correctlyParsedBody
          case id_ of
            0 -> do
              logError
                hLogger
                "  There is no user with this ID, or the user is already the author"
              respond
                (responseLBS status500 [("Content-Type", "text/plain")] "")
            _ ->
              respond
                (responseLBS created201 [("Content-Type", "text/plain")] "")
    -- show author, like
    -- http://localhost:3000/author/<token>/<id''>
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id''"
      authorMb <- findAuthorByID hDb pool id_
      case authorMb of
        Nothing -> do
          logError hLogger "  Author not exist"
          respond
            (responseLBS
               notFound404
               [("Content-Type", "text/plain")]
               "author not exist")
        Just author -> do
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $
             encode author)
    -- delete author (see example)
    delete = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id''"
      deleteByID hDb pool "author" id_
      respond (responseLBS status204 [("Content-Type", "text/plain")] "")
    -- author editing (see example)
    put = do
      let id_ = toId req
          descrMb = toParam req "description"
      when (id_ == 0) $ do logError hLogger "  Invalid id''"
      case descrMb of
        Nothing -> do
          logError hLogger "  The \"description\" parameter is required"
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Just descr -> do
          updateByID hDb pool "author" id_ descr
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $
             encode (RawAuthor id_ $ T.pack descr))
