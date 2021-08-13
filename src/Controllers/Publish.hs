{-# LANGUAGE OverloadedStrings #-}

module Controllers.Publish where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Servises.Db
import Servises.Logger
import Servises.Token

-- publication of a draft, like
-- http://localhost:3000/publish/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
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
        Just (id_author, _) -> do
            id_ <- publishPost hDb pool (toId req) id_author
            case id_ of
                0 -> do
                    logError hLogger "  Draft not found"
                    respond (responseLBS status400 [("Content-Type", "text/plain")] "")
                _ -> respond (responseLBS status201 [("Content-Type", "text/plain")] "")
