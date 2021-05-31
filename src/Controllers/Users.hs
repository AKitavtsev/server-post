{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Users 
    where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
import Data.Pool (Pool)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time

import Control.Monad (when)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import Controllers.Token (Token (..))
import FromRequest
import Models.User
import Servises.Config
import Servises.Logger
import Servises.Token
import Servises.Db


-- routes :: Pool Connection -> Request
       -- -> (Response -> IO ResponseReceived)
       -- -> IO ResponseReceived
routes pool hLogger hToken hDb req respond = do
  logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))
  case  toMethod req of
    "POST"   -> post    
    "GET"    -> get
    "DELETE" -> delete
  where 
    post = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ (BL.unpack body))
      case eitherDecode body :: Either String UserIn of
        Left e -> do
          logError hLogger "  Invalid request body"         
          respond (responseLBS status400 [("Content-Type", "text/plain")] $ BL.pack e)
        Right correctlyParsedBody -> do
-- проверяю наличие логина в юзерс (а может не надо, пусть БД разбирается)
          -- exL <- liftIO $ existLogin hDb pool (login correctlyParsedBody)
          -- case exL of
            -- False -> do
              c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
              id <- insertUser hDb pool correctlyParsedBody c_date 
              insertImage hDb pool correctlyParsedBody id
              token <- (createToken hToken) id False
              -- insertUser hDb pool correctlyParsedBody c_date 
              -- Just (id, adm) <- liftIO $ findUserByLogin hDb pool 
                       -- (login correctlyParsedBody) 
                       -- (Models.User.password correctlyParsedBody)
              -- insertImage hDb pool correctlyParsedBody id
              -- token <- (createToken hToken) id adm
              respond (responseLBS created201 
                           [("Content-Type", "text/plain")] $ encode (Token token))
            -- True -> do
              -- logError hLogger "  Login already exists"
              -- respond (responseLBS found302 [("Content-Type", "text/plain")] "user exist")
    get = do
      vt <- validToken hToken (toToken req)
      case vt of
        Just (id, _) -> do
          when (id == 0) $ do
            logError hLogger "  Invalid id"
          userMb <- liftIO $ findUserByID hDb pool id
          case userMb of
            Nothing -> do
              logError hLogger "  User not exist"
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "user not exist")
            Just user -> do 
              respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode user)
        Nothing -> do
          logError hLogger "  Invalid or outdated token"
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")        
    delete = do
      let id    = toId req
      vt <- validToken hToken (toToken req)
      case  vt of
        Nothing -> do
          logError hLogger "  Invalid or outdated token"
          respond (responseLBS status400 [("Content-Type", "text/plain")] "bad")
        Just (_, True) -> do
          -- deleteUserByID hDb pool id
          deleteByID hDb pool "user" id
          respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")

curTimeStr :: String -> IO String
curTimeStr form = do
    utc <- Time.getCurrentTime
    return (Time.formatTime Time.defaultTimeLocale form utc)


