{-# LANGUAGE OverloadedStrings #-}

module Router (routes)
  where

import FromRequest (toPath)

import qualified Controllers.Users
import qualified Controllers.Images
import qualified Controllers.Token

import Network.Wai
-- import Network.Wai.Handler.Warp
import Network.HTTP.Types
    
routes pool hLogger hToken hDb req respond  = do
    case toPath req of
        "user"  -> do
          Controllers.Users.routes pool hLogger hToken hDb req respond
        "token" -> do
          Controllers.Token.routes pool hLogger hToken hDb req respond
        "image" -> do
          Controllers.Images.routes pool hLogger hToken hDb req respond
        _       -> do
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
  

