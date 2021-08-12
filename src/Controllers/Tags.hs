{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags (routes) where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode)
import Data.Pool (Pool)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Control.Monad (when)
import Database.PostgreSQL.Simple (Connection (..))
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Tag
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
    -- tag creation (see example)
    post vt = do
        case vt of
            Just (_, True) -> do
                body <- strictRequestBody req
                logDebug hLogger ("  Body = " ++ (BL.unpack body))
                case eitherDecode body :: Either String Tag of
                    Left e -> do
                        logError hLogger ("  Invalid request body  - " ++ e)
                        respond (responseLBS status400 [("Content-Type", "text/plain")] "")
                    Right correctlyParsedBody -> do
                        insertTag hDb pool correctlyParsedBody
                        respond (responseLBS created201 [("Content-Type", "text/plain")] "")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
    -- show tag, like
    -- http://localhost:3000/tag/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
        let id = toId req
        when (id == 0) $ do
            logError hLogger "  Invalid id"
        tagMb <- liftIO $ findTagByID hDb pool id
        case tagMb of
            Nothing -> do
                logError hLogger "  Tag not exist"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
            Just tag -> do
                respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode tag)
    -- deleting a tag
    delete vt = do
        case vt of
            Just (_, True) -> do
                let id = toId req
                when (id == 0) $ do
                    logError hLogger "  Invalid id"
                deleteByID hDb pool "tag" id
                respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
    -- tag editing
    put vt = do
        case vt of
            Just (_, True) -> do
                let id = toId req
                when (id == 0) $ do
                    logError hLogger "  Invalid id"
                let tagMb = (toParam req "tag")
                when (not (tagMb == Nothing)) $ do
                    let tag = case tagMb of Just t -> t
                    logError hLogger ("  Update tag to " ++ tag)
                    updateByID hDb pool "tag" id "tag" tag
                respond (responseLBS status200 [("Content-Type", "text/plain")] "")
            Just (_, False) -> do
                logError hLogger "  Administrator authority required"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
