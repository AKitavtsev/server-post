{-# LANGUAGE OverloadedStrings #-}

module Router
  ( routes
  ) where

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

import Services.Db
import Services.Logger
import Services.Token

import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal
import Network.HTTP.Types
import Network.Wai


routes ::
     Pool Connection
  -> Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  logInfo hLogger ("  Path = " ++ toPath req)
  case toPath req of
    "user" -> Controllers.Users.routes pool hLogger hToken hDb req respond
    "token" -> Controllers.Token.routes pool hLogger hToken hDb req respond
    "image" -> Controllers.Images.routes pool hLogger hDb req respond
    "author" -> Controllers.Authors.routes pool hLogger hToken hDb req respond
    "category" ->
      Controllers.Categories.routes pool hLogger hToken hDb req respond
    "tag" -> Controllers.Tags.routes pool hLogger hToken hDb req respond
    "draft" -> Controllers.Drafts.routes pool hLogger hToken hDb req respond
    "photo" -> Controllers.Photos.routes pool hLogger hToken hDb req respond
    "publish" -> Controllers.Publish.routes pool hLogger hToken hDb req respond
    "comment" -> Controllers.Comments.routes pool hLogger hToken hDb req respond
    "posts" -> Controllers.Posts.routes pool hLogger hToken hDb req respond
    _ -> do
      logError hLogger "  Path not found"
      respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
