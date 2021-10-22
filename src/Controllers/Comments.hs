{-# LANGUAGE OverloadedStrings #-}

module Controllers.Comments
  ( routes
  ) where

import Data.Aeson (eitherDecode)

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

routes :: Monad m =>
     Services.Logger.Handle m
  -> Services.Token.Handle m
  -> Services.Db.Handle m
  -> FromRequest.HandleRequst m
  -> Request
  -> (Response -> m b)
  -> m b
routes hLogger hToken hDb hRequest req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing ->
      respondWithError hLogger respond status400 "  Invalid or outdated token"
    Just (id_author, adm) -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post id_author
        "DELETE" -> delete id_author adm
        _ -> respondWithError hLogger respond status404 "  Invalid method"
    -- comment creation (see example)
  where
    post id_author = do
      body <- toBody hRequest req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String RawComment of
        Right correctlyParsedBody -> do
          creation_date <- curTimeStr hToken
          id_ <- insertComment hDb correctlyParsedBody id_author creation_date
          case id_ of
            0 -> respondWithError hLogger respond status404 "  post not found"
            _ -> respondWithSuccess respond status201 (IdComment id_)
        Left e ->
          respondWithError
            hLogger
            respond
            status400
            ("  invalid request body  - " ++ e)
    -- deleting a comment
    delete id_author adm = do
      let id_ = toId req
      case id_ of
        0 -> respondWithError hLogger respond status400 "  Invalid id"
        _ -> do
          (if adm
             then deleteByID hDb "comment" id_
             else deleteComment hDb id_ id_author)
          respondWithSuccess respond status204 ("" :: String)
