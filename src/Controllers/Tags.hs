{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags
  ( routes
  ) where

import Data.Aeson (eitherDecode)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Monad (unless, when)
import Data.Maybe (fromMaybe, isNothing)
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Tag
import Services.Db
import Services.Logger
import Services.Token
import Utils

routes ::
     Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing ->
      respondWithError hLogger respond status400 "  Invalid or outdated token"
    _ -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post vt
        "GET" -> get
        "DELETE" -> delete vt
        "PUT" -> put vt
        _ -> respondWithError hLogger respond status404 "  Invalid method"
    -- tag_ creation (see example)
  where
    post vt = do
      case vt of
        Just (_, True) -> do
          body <- strictRequestBody req
          logDebug hLogger ("  Body = " ++ BL.unpack body)
          case eitherDecode body :: Either String Tag of
            Left e ->
              respondWithError
                hLogger
                respond
                status400
                ("  Invalid request body  - " ++ e)
            Right correctlyParsedBody -> do
              insertTag hDb correctlyParsedBody
              respondWithSuccess respond status201 ("" :: String)
        Just (_, False) ->
          respondWithError
            hLogger
            respond
            status404
            "  Administrator authority required"
        Nothing -> respondWithError hLogger respond status404 ""
    -- show tag_, like
    -- http://localhost:3000/tag_/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      tagMb <- findTagByID hDb id_
      case tagMb of
        Nothing -> respondWithError hLogger respond status404 "  Tag not exist"
        Just tag_ -> respondWithSuccess respond status201 tag_
    -- deleting a tag_
    delete vt = do
      case vt of
        Just (_, True) -> do
          let id_ = toId req
          when (id_ == 0) $ do logError hLogger "  Invalid id_"
          deleteByID hDb "tag_" id_
          respondWithSuccess respond status204 ("" :: String)
        Just (_, False) ->
          respondWithError
            hLogger
            respond
            status404
            "  Administrator authority required"
        Nothing -> respondWithError hLogger respond status404 ""
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
            updateByID hDb "tag_" id_ tag_
          respondWithSuccess respond status200 ("" :: String)
        Just (_, False) ->
          respondWithError
            hLogger
            respond
            status404
            "  Administrator authority required"
        Nothing -> respondWithError hLogger respond status404 ""
