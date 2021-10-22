{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories
  ( routes
  ) where

import Control.Monad (unless, when)
import Data.Aeson (eitherDecode)
import Data.Maybe (fromMaybe, isNothing)
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import FromRequest
import Models.Category
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
    _ -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post vt
        "GET" -> get
        "DELETE" -> delete vt
        "PUT" -> put vt
        _ -> respondWithError hLogger respond status404 "  Invalid method"
    -- caterories creation (see example)
  where
    post (Just (_, True)) = do
      body <- toBody hRequest req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String Category of
        Left e ->
          respondWithError
            hLogger
            respond
            status400
            ("  Invalid method request body  - " ++ e)
        Right correctlyParsedBody -> do
          id_ <- insertCategory hDb correctlyParsedBody
          case id_ of
            0 ->
              respondWithError
                hLogger
                respond
                status500
                "  category - parent not found"
            _ -> respondWithSuccess respond status201 ("" :: String)
    post (Just (_, False)) =
      respondWithError hLogger respond status404 "  Administrator authority required"
    post Nothing =
      respondWithError hLogger respond status400 "  Invalid or outdated token"
    -- show category, like
    -- http://localhost:3000/category/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id"
      categoryMb <- findCategoryByID hDb id_
      case categoryMb of
        Nothing ->
          respondWithError hLogger respond status400 "  Category not exist"
        Just category -> respondWithSuccess respond status201 category
    -- deleting a caterory  (see example)
    delete (Just (_, True)) = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id"
      deleteByID hDb "category" id_
      respondWithSuccess respond status201 ("" :: String)
    delete (Just (_, False)) =
      respondWithError hLogger respond status400 "  Administrator authority required"
    delete Nothing = respondWithError hLogger respond status404 "  no admin"
    -- category editing
    put (Just (_, True)) = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id"
      let nameMb = toParam req "name_"
      unless (isNothing nameMb) $ do
        let name_ = fromMaybe "" nameMb
        updateByID hDb "category" id_ name_
        logDebug hLogger ("  Update name_ to " ++ name_)
      let parentMb = toParam req "id_parent"
      case parentMb of
        Nothing -> respondWithSuccess respond status200 ("" :: String)
        Just parent -> do
          id' <- updateOwnerCategory hDb id_ parent
          logDebug hLogger ("  Update id_parent to " ++ parent)
          case id' of
            0 -> respondWithError hLogger respond status500 "  category - parent not found"
            _ -> respondWithSuccess respond status200 ("" :: String)
    put (Just (_, False)) =
      respondWithError hLogger respond status404 "  Administrator authority required"
    put Nothing = respondWithError hLogger respond status404 "  no admin"
