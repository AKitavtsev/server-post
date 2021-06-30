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
      id <- updatePost (toId req) id_author
      case id of
        0 -> do
          logError hLogger "  Draft not found" 
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        _ -> respond (responseLBS status201 [("Content-Type", "text/plain")] "")

    where 
      updatePost id_dr id_author   = 
        return (id_dr, id_author)  >>= 
        takeWholeDraft hDb pool    >>=
        publishPost    hDb pool
      




















      
