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
routes :: Monad m =>
     Services.Logger.Handle m
  -> Services.Token.Handle m
  -> Services.Db.Handle m
  -> Request
  -> (Response -> m b)
  -> m b
routes hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing ->
      respondWithError hLogger respond status401 "  Invalid or outdated token"
    Just (id_author, _) -> do
      id_ <- publishPost hDb (toId req) id_author
      case id_ of
        0 -> respondWithError hLogger respond status404 "  Draft not found"
        _ -> respondWithSuccess respond status200 ("" :: String)
