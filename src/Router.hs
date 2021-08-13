{-# LANGUAGE OverloadedStrings #-}

module Router (routes) where

import FromRequest (toPath)

import qualified Controllers.Authors
import qualified Controllers.Categories
import qualified Controllers.Comments
import qualified Controllers.Drafts
import qualified Controllers.Images
import qualified Controllers.Photos
import qualified Controllers.Posts
import qualified Controllers.Publish
import qualified Controllers.Tags
import qualified Controllers.Token
import qualified Controllers.Users

import Servises.Logger
import Servises.Token
import Servises.Db

import Network.HTTP.Types
import Network.Wai
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import qualified Data.Text as T

routes :: Connection
                -> Pool Connection
                -> Servises.Logger.Handle
                -> Servises.Token.Handle
                -> Servises.Db.Handle
                -> Request
                -> (Response -> IO b)
                -> IO b
routes conn pool hLogger hToken hDb req respond = do
    logInfo hLogger ("  Path = " ++ T.unpack (toPath req))
    case toPath req of
        "user" -> Controllers.Users.routes pool hLogger hToken hDb req respond
        "token" -> Controllers.Token.routes pool hLogger hToken hDb req respond
        "image" -> Controllers.Images.routes pool hLogger hDb req respond
        "author" -> Controllers.Authors.routes pool hLogger hToken hDb req respond
        "category" -> Controllers.Categories.routes pool hLogger hToken hDb req respond
        "tag" -> Controllers.Tags.routes pool hLogger hToken hDb req respond
        "draft" -> Controllers.Drafts.routes pool hLogger hToken hDb req respond
        "photo" -> Controllers.Photos.routes pool hLogger hToken hDb req respond
        "publish" -> Controllers.Publish.routes pool hLogger hToken hDb req respond
        "comment" -> Controllers.Comments.routes pool hLogger hToken hDb req respond
        "posts" -> Controllers.Posts.routes pool hLogger hToken hDb req respond
        "migration" -> do
            runMigrations hDb hLogger conn pool "sql"
            respond $ responseLBS status200 [("Content-Type", "text/plain")] ""
        _ -> do
            logError hLogger "  Path not found"
            respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
