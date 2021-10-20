{-# LANGUAGE OverloadedStrings #-}

module Controllers.Publish
  ( routes
  ) where

import Network.HTTP.Types
import Network.Wai

import FromRequest
import Services.Db
import Services.Logger
import Services.Token
import Utils

-- publication of a draft, like
-- http://localhost:3000/publish/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
routes ::
     Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle IO
  -> Request
  -> (Response -> IO b)
  -> IO b
routes hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing ->
      respondWithError hLogger respond status400 "  Invalid or outdated token"
    Just (id_author, _) -> do
      id_ <- publishPost hDb (toId req) id_author
      case id_ of
        0 -> respondWithError hLogger respond status400 "  Draft not found"
        _ -> respondWithSuccess respond status201 ("" :: String)
