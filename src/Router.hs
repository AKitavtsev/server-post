{-# LANGUAGE OverloadedStrings #-}

module Router (routes)
  where

import FromRequest (toPath)

import qualified Controllers.Authors
import qualified Controllers.Categories
import qualified Controllers.Comments
import qualified Controllers.Drafts
import qualified Controllers.Images
import qualified Controllers.Photos
import qualified Controllers.Publish
import qualified Controllers.Tags
import qualified Controllers.Token
import qualified Controllers.Users

import Servises.Logger

import Network.Wai
-- import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.Text as T
    
routes pool hLogger hToken hDb req respond  = do
    logInfo hLogger ("  Path = " ++ (T.unpack $ toPath req))
    case toPath req of
        "user"       -> Controllers.Users.routes pool hLogger hToken hDb req respond
        "token"      -> Controllers.Token.routes pool hLogger hToken hDb req respond
        "image"      -> Controllers.Images.routes pool hLogger hToken hDb req respond
        "author"     -> Controllers.Authors.routes pool hLogger hToken hDb req respond
        "category"   -> Controllers.Categories.routes pool hLogger hToken hDb req respond
        "tag"        -> Controllers.Tags.routes pool hLogger hToken hDb req respond
        "draft"      -> Controllers.Drafts.routes pool hLogger hToken hDb req respond
        "photo"      -> Controllers.Photos.routes pool hLogger hToken hDb req respond
        "publish"    -> Controllers.Publish.routes pool hLogger hToken hDb req respond
        "comment"    -> Controllers.Comments.routes pool hLogger hToken hDb req respond
        _            -> do
          logError hLogger "  Path not found"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
  

