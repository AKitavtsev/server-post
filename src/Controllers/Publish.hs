{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Controllers.Publish 
    where

import Network.HTTP.Types
import Network.Wai

import FromRequest
import Servises.Logger
import Servises.Token
import Servises.Db

-- publication of a draft, like
-- http://localhost:3000/publish/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
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
