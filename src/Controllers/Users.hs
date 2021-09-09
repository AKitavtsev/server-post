{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users
  ( routes
  ) where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode)

import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)

import Control.Monad (when)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.User
import Services.Db
import Services.Logger
import Services.Token

routes ::
     Pool Connection
  -> Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
  case toMethod req of
    "POST" -> post
    "GET" -> get
    "DELETE" -> delete
    _ -> do
      logError hLogger "  Invalid method"
      respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
    -- user creation (see example)
  where
    post = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ BL.unpack body)
      case eitherDecode body :: Either String UserIn of
        Left e -> do
          logError hLogger ("  Invalid request body - " ++ e)
          respond
            (responseLBS status400 [("Content-Type", "text/plain")] $ BL.pack e)
        Right correctlyParsedBody -> do
          c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
          id_ <- insertUser hDb pool correctlyParsedBody c_date
          case id_ of
            0 -> do
              logError hLogger "  Login already exist"
              respond
                (responseLBS status400 [("Content-Type", "text/plain")] "")
            _ -> do
              idim <- insertImage hDb pool correctlyParsedBody id_
              when (idim == 0) $ do
                logWarning
                  hLogger
                  "  Invalid image type specified (only png, jpg, gif or bmp is allowed)"
              token_ <- createToken hToken id_ False
              respond
                (responseLBS created201 [("Content-Type", "text/plain")] $
                 encode (UserID id_ token_))
    -- show user, like
    -- http://localhost:3000/user/1.120210901202553ff034f3847c1d22f091dde7cde045264
    get = do
      vt <- validToken hToken (toToken req)
      case vt of
        Just (id_, _) -> do
          when (id_ == 0) $ do logError hLogger "  Invalid id_"
          userMb <- liftIO $ findUserByID hDb pool id_
          case userMb of
            Nothing -> do
              logError hLogger "  User not exist"
              respond
                (responseLBS
                   notFound404
                   [("Content-Type", "text/plain")]
                   "user not exist")
            Just user -> do
              respond
                (responseLBS status200 [("Content-Type", "text/plain")] $
                 encode user)
        Nothing -> do
          logError hLogger "  Invalid or outdated token"
                -- delete user (see example)
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    delete = do
      let id_ = toId req
      vt <- validToken hToken (toToken req)
      case vt of
        Nothing -> do
          logError hLogger "  Invalid or outdated token"
          respond (responseLBS status400 [("Content-Type", "text/plain")] "bad")
        Just (_, True)
                -- deleteUserByID hDb pool id_
         -> do
          deleteByID hDb pool "user" id_
          respond
            (responseLBS status204 [("Content-Type", "text/plain")] "delete")
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond
            (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")
