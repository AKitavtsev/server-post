{-# LANGUAGE OverloadedStrings #-}

module Controllers.Comments
  ( routes
  ) where

import Data.Aeson (eitherDecode)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Comment
import Services.Db
import Services.Logger
import Services.Token
import Utils

routes ::
     Pool Connection
  -> Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> respondWithError hLogger respond status400 "  Invalid or outdated token"
    Just (id_author, adm) -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post id_author
        "DELETE" -> delete id_author adm
        _ -> respondWithError hLogger respond status404 "  Invalid method"
    -- comment creation (see example)
  where
    post id_author = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String RawComment of
        Right correctlyParsedBody -> do
          c_date <- curTimeStr
          id_ <- insertComment hDb pool correctlyParsedBody id_author c_date
          case id_ of
            0 -> respondWithError hLogger respond status404 "  post not found"
            _ -> respondWithSuccess respond status201 (IdComment id_)
        Left e -> respondWithError hLogger respond status400 
                    ("  invalid request body  - " ++ e)
    -- deleting a comment
    delete id_author adm = do
      let id_ = toId req
      case id_ of
        0 -> respondWithError hLogger respond status400 "  Invalid id_"
        _ -> do
          (if adm then deleteByID hDb pool "comment" id_
           else deleteComment hDb pool id_ id_author)      
          respondWithSuccess respond status204 ("" :: String)
