{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images
  ( routes
  ) where

import FromRequest
import Services.Db
import Services.Logger
import Utils

import Control.Monad (when)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import Network.HTTP.Types
import Network.Wai

-- show avatar, like
-- http://localhost:3000/image/1
routes ::
     Pool Connection
  -> Services.Logger.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hDb req respond = do
  let id_ = toIdImage req
  when (id_ == 0) $ logError hLogger "  Invalid id"
  imageMb <- findImageByID hDb pool id_
  case imageMb of
    Nothing -> respondWithError hLogger respond status404 "  Image not found"
    Just imageAndType -> respondWithImage respond imageAndType
    