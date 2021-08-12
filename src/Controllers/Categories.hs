{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories (routes) where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Monad (when)
import Database.PostgreSQL.Simple (Connection (..))
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Category
import Servises.Db
import Servises.Logger
import Servises.Token

routes pool hLogger hToken hDb req respond = do
    vt <- validToken hToken (toToken req)
    case vt of
        Nothing -> do
            logError hLogger "  Invalid or outdated token"
            respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        _ -> do
            logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))
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
                logDebug hLogger ("  Body = " ++ (BL.unpack body))
                case eitherDecode body :: Either String Category of
                    Left e -> do
                        logError hLogger ("  Invalid request body  - " ++ e)
                        respond (responseLBS status400 [("Content-Type", "text/plain")] "")
                    Right correctlyParsedBody -> do
                        id <- insertCategory hDb pool correctlyParsedBody
                        case id of
                            0 -> do
                                logError hLogger "  category - owner not found"
                                respond (responseLBS status500 [("Content-Type", "text/plain")] "")
                            _ -> respond (responseLBS created201 [("Content-Type", "text/plain")] "")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
    -- show category, like
    -- http://localhost:3000/category/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
        let id = toId req
        when (id == 0) $ do
            logError hLogger "  Invalid id"
        categoryMb <- liftIO $ findCategoryByID hDb pool id
        case categoryMb of
            Nothing -> do
                logError hLogger "  Category not exist"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
            Just category -> do
                -- ac <- allCategories [id] [id]
                -- logDebug hLogger (show ac)
                respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode category)
    -- deleting a caterory  (see example)
    delete vt = do
        case vt of
            Just (_, True) -> do
                let id = toId req
                when (id == 0) $ do
                    logError hLogger "  Invalid id"
                deleteByID hDb pool "category" id
                respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
    -- category editing
    put vt = do
        case vt of
            Just (_, True) -> do
                let id = toId req
                when (id == 0) $ do
                    logError hLogger "  Invalid id"
                let nameMb = (toParam req "name")
                when (not (nameMb == Nothing)) $ do
                    let name = case nameMb of Just n -> n
                    updateByID hDb pool "category" id "name" name
                    logDebug hLogger ("  Update name to " ++ name)
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
                        id <- updateOwnerCategory hDb pool id owner
                        logDebug hLogger ("  Update id_owner to " ++ owner)
                        case id of
                            0 -> do
                                logError hLogger "  category - owner not found"
                                respond (responseLBS status500 [("Content-Type", "text/plain")] "")
                            _ -> respond (responseLBS status200 [("Content-Type", "text/plain")] "")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
