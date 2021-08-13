{-# LANGUAGE OverloadedStrings #-}

module Controllers.Comments where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Comment
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
        Just (id_author, adm) -> do
            logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
            case toMethod req of
                "POST" -> post id_author
                "DELETE" -> delete id_author adm
                _ -> do
                    logError hLogger "  Invalid method"
                    respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
  where
    -- comment creation (see example)
    post id_author = do
        body <- strictRequestBody req
        logDebug hLogger ("  Body = " ++ BL.unpack body)
        case eitherDecode body :: Either String CommentIn of
            Right correctlyParsedBody -> do
                c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
                id_ <- insertComment hDb pool correctlyParsedBody id_author c_date
                case id_ of
                    0 -> do
                        logError hLogger "  post not found"
                        respond (responseLBS status400 [("Content-Type", "text/plain")] "")
                    _ ->
                        respond
                            ( responseLBS status201 [("Content-Type", "text/plain")] $
                                encode (CommentID id_)
                            )
            Left e -> do
                logError hLogger ("  Invalid request body  - " ++ e)
                respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    -- deleting a comment
    delete id_author adm = do
        let id_ = toId req
        case id_ of
            0 -> do
                logError hLogger "  Invalid id_"
                respond (responseLBS status400 [("Content-Type", "text/plain")] "")
            _ -> do
                case adm of
                    True -> deleteByID hDb pool "comment" id_
                    False -> deleteComment hDb pool id_ id_author
                respond (responseLBS status204 [("Content-Type", "text/plain")] "")
