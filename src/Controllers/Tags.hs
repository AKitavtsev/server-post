{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags
  ( routes
  ) where

import Data.Aeson (eitherDecode, encode)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Monad (unless, when)
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Tag
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
    _ -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post vt
        "GET" -> get
        "DELETE" -> delete vt
        "PUT" -> put vt
        _ -> do
          logError hLogger "  Invalid method"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
    -- tag_ creation (see example)
  where
    post vt = do
      case vt of
        Just (_, True) -> do
          body <- strictRequestBody req
          logDebug hLogger ("  Body = " ++ BL.unpack body)
          case eitherDecode body :: Either String Tag of
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)
              respond
                (responseLBS status400 [("Content-Type", "text/plain")] "")
            Right correctlyParsedBody -> do
              insertTag hDb pool correctlyParsedBody
              respond
                (responseLBS created201 [("Content-Type", "text/plain")] "")
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond
            (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
        Nothing ->
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
    -- show tag_, like
    -- http://localhost:3000/tag_/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      tagMb <- findTagByID hDb pool id_
      case tagMb of
        Nothing -> do
          logError hLogger "  Tag not exist"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
        Just tag_ -> do
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $
             encode tag_)
    -- deleting a tag_
    delete vt = do
      case vt of
        Just (_, True) -> do
          let id_ = toId req
          when (id_ == 0) $ do logError hLogger "  Invalid id_"
          deleteByID hDb pool "tag_" id_
          respond
            (responseLBS status204 [("Content-Type", "text/plain")] "delete")
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond
            (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
        Nothing ->
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
    -- tag_ editing
    put vt = do
      case vt of
        Just (_, True) -> do
          let id_ = toId req
          when (id_ == 0) $ do logError hLogger "  Invalid id_"
          let tagMb = toParam req "tag_"
          unless (isNothing tagMb) $ do
            let tag_ = fromMaybe "" tagMb
            logError hLogger ("  Update tag_ to " ++ tag_)
            updateByID hDb pool "tag_" id_ tag_
          respond (responseLBS status200 [("Content-Type", "text/plain")] "")
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond
            (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
        Nothing ->
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
