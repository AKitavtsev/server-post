{-# LANGUAGE OverloadedStrings #-}

module Router
  ( routes
  ) where

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

import FromRequest
import Services.Db
import Services.Logger
import Services.Token
import Utils

import Network.HTTP.Types
import Network.Wai


routes :: Monad m =>
     Services.Logger.Handle m
  -> Services.Token.Handle m
  -> Services.Db.Handle m
  -> FromRequest.HandleRequest m  
  -> Request
  -> (Response -> m b)
  -> m b
routes hLogger hToken hDb hRequest req respond = do
  logInfo hLogger ("  Path = " ++ toPath req)
  case toPath req of
    "author" -> Controllers.Authors.routes hLogger hToken hDb hRequest req respond
    "category" -> Controllers.Categories.routes hLogger hToken hDb hRequest req respond
    "comment" -> Controllers.Comments.routes hLogger hToken hDb hRequest req respond
    "draft" -> Controllers.Drafts.routes hLogger hToken hDb hRequest req respond
    "image" -> Controllers.Images.routes hLogger hDb req respond
    "photo" -> Controllers.Photos.routes hLogger hToken hDb hRequest req respond
    "posts" -> Controllers.Posts.routes hLogger hToken hDb req respond
    "publish" -> Controllers.Publish.routes hLogger hToken hDb req respond
    "tag" -> Controllers.Tags.routes hLogger hToken hDb hRequest req respond
    "token" -> Controllers.Token.routes hLogger hToken hDb req respond
    "user" -> Controllers.Users.routes hLogger hToken hDb hRequest req respond
    _ -> respondWithError hLogger respond status404 "  Path not found"
