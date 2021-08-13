{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories (routes) where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode)
import Data.Pool (Pool)
import Data.Maybe (isNothing, fromMaybe)
import Control.Monad (when, unless)
import Database.PostgreSQL.Simple.Internal
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import FromRequest
import Models.Category
import Servises.Db
import Servises.Logger
import Servises.Token

routes :: Pool Connection
                -> Servises.Logger.Handle
                -> Servises.Token.Handle
                -> Servises.Db.Handle
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
  where
    -- caterories creation (see example)
    post vt = do
        case vt of
            Just (_, True) -> do
                body <- strictRequestBody req
                logDebug hLogger ("  Body = " ++ BL.unpack body)
                case eitherDecode body :: Either String Category of
                    Left e -> do
                        logError hLogger ("  Invalid request body  - " ++ e)
                        respond (responseLBS status400 [("Content-Type", "text/plain")] "")
                    Right correctlyParsedBody -> do
                        id_ <- insertCategory hDb pool correctlyParsedBody
                        case id_ of
                            0 -> do
                                logError hLogger "  category - owner not found"
                                respond (responseLBS status500 [("Content-Type", "text/plain")] "")
                            _ -> respond (responseLBS created201 [("Content-Type", "text/plain")] "")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
            Nothing -> respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
    -- show category, like
    -- http://localhost:3000/category/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
        let id_ = toId req
        when (id_ == 0) $ do
            logError hLogger "  Invalid id_"
        categoryMb <- liftIO $ findCategoryByID hDb pool id_
        case categoryMb of
            Nothing -> do
                logError hLogger "  Category not exist"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
            Just category -> do
                -- ac <- allCategories [id_] [id_]
                -- logDebug hLogger (show ac)
                respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode category)
    -- deleting a caterory  (see example)
    delete vt = do
        case vt of
            Just (_, True) -> do
                let id_ = toId req
                when (id_ == 0) $ do
                    logError hLogger "  Invalid id_"
                deleteByID hDb pool "category" id_
                respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 
                         [("Content-Type", "text/plain")] "no admin")
            Nothing -> respond (responseLBS notFound404 
                                [("Content-Type", "text/plain")] "no admin")
    -- category editing
    put vt = do
        case vt of
            Just (_, True) -> do
                let id_ = toId req
                when (id_ == 0) $ do
                    logError hLogger "  Invalid id_"
                let nameMb = (toParam req "name_")
                unless (isNothing nameMb) $ do
                    let name_ = fromMaybe "" nameMb
                    updateByID hDb pool "category" id_ name_
                    logDebug hLogger ("  Update name_ to " ++ name_)
                let ownerMb = (toParam req "id_owner")
                case ownerMb of
                    Nothing ->
                        respond
                            ( responseLBS
                                status200
                                [("Content-Type", "text/plain")]
                                ""
                            )
                    Just owner -> do
                        id' <- updateOwnerCategory hDb pool id_ owner
                        logDebug hLogger ("  Update id_owner to " ++ owner)
                        case id' of
                            0 -> do
                                logError hLogger "  category - owner not found"
                                respond (responseLBS status500 [("Content-Type", "text/plain")] "")
                            _ -> respond (responseLBS status200 [("Content-Type", "text/plain")] "")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
            Nothing -> respond (responseLBS notFound404
                               [("Content-Type", "text/plain")] "no admin")