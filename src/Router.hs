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
import Utils

import Network.HTTP.Types
import Network.Wai

import qualified Data.Text as T

routes ::
     Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes hLogger hToken hDb req respond = do
  logInfo hLogger ("  Path = " ++ T.unpack (toPath req))
  case toPath req of
    "author" -> Controllers.Authors.routes hLogger hToken hDb req respond
    "category" -> Controllers.Categories.routes hLogger hToken hDb req respond
    "comment" -> Controllers.Comments.routes hLogger hToken hDb req respond
    "draft" -> Controllers.Drafts.routes hLogger hToken hDb req respond
    "image" -> Controllers.Images.routes hLogger hDb req respond
    "photo" -> Controllers.Photos.routes hLogger hToken hDb req respond
    "posts" -> Controllers.Posts.routes hLogger hToken hDb req respond
    "publish" -> Controllers.Publish.routes hLogger hToken hDb req respond
    "tag" -> Controllers.Tags.routes hLogger hToken hDb req respond
    "token" -> Controllers.Token.routes hLogger hToken hDb req respond
    "user" -> Controllers.Users.routes hLogger hToken hDb req respond
    _ -> respondWithError hLogger respond status404 "  Path not found"
