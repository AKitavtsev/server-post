{-# LANGUAGE OverloadedStrings #-}

module Router (routes)
  where

import FromRequest (toPath)

import qualified Controllers.Images
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
        "user"  -> do
          Controllers.Users.routes pool hLogger hToken hDb req respond
        "token" -> do
          Controllers.Token.routes pool hLogger hToken hDb req respond
        "image" -> do
          Controllers.Images.routes pool hLogger hToken hDb req respond
        _       -> do
          logError hLogger "  Path not found"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
  

