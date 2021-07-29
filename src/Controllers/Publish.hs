{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Controllers.Publish 
    where

import Data.Pool (Pool)

import Network.HTTP.Types
import Network.Wai

import FromRequest
import Servises.Logger
import Servises.Token
import Servises.Db

routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (id_author, _) -> do
      id <- publishPost hDb pool (toId req) id_author
      case id of
        0 -> do
          logError hLogger "  Draft not found" 
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        _ -> respond (responseLBS status201 [("Content-Type", "text/plain")] "")
