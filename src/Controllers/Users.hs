{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users
  ( routes
  ) where

import Data.Aeson (eitherDecode)

import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)

import Control.Monad (when)
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.User
import Services.Db
import Services.Logger
import Services.Token
import Utils

routes ::
     Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle IO
  -> Request
  -> (Response -> IO b)
  -> IO b
routes hLogger hToken hDb req respond = do
  logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
  case toMethod req of
    "POST" -> post
    "GET" -> get
    "DELETE" -> delete
    _ -> respondWithError hLogger respond status404 "  Invalid method"
    -- user creation (see example)
  where
    post = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String RawUser of
        Left e ->
          respondWithError
            hLogger
            respond
            status400
            ("  invalid request body  - " ++ e)
        Right correctlyParsedBody -> do
          c_date <- curTimeStr
          id_ <- insertUser hDb correctlyParsedBody c_date
          case id_ of
            0 ->
              respondWithError hLogger respond status400 "  Login already exist"
            _ -> do
              idim <- insertImage hDb correctlyParsedBody id_
              when (idim == 0) $ do
                logWarning
                  hLogger
                  "  Invalid image type specified (only png, jpg, gif or bmp is allowed)"
              token_ <- createToken hToken id_ False
              respondWithSuccess respond status201 (TokenForUser id_ token_)
-- show user, like
-- http://localhost:3000/user/1.120210901202553ff034f3847c1d22f091dde7cde045264
    get = do
      vt <- validToken hToken (toToken req)
      case vt of
        Just (id_, _) -> do
          when (id_ == 0) $ do logError hLogger "  Invalid id_"
          userMb <- findUserByID hDb id_
          case userMb of
            Nothing ->
              respondWithError hLogger respond status400 "  User not exist"
            Just user -> respondWithSuccess respond status200 user
        Nothing ->
          respondWithError
            hLogger
            respond
            status400
            "  Invalid or outdated token"
-- delete user (see example)
    delete = do
      let id_ = toId req
      vt <- validToken hToken (toToken req)
      case vt of
        Nothing ->
          respondWithError
            hLogger
            respond
            status400
            "  Invalid or outdated token"
        Just (_, True) -> do
          deleteByID hDb "user" id_
          respondWithSuccess respond status204 ("" :: String)
        Just (_, False) ->
          respondWithError
            hLogger
            respond
            status400
            "  Administrator authority required"
