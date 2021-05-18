{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Users 
    where

-- import Control.Monad (when)
import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
-- import Database.PostgreSQL.Simple
-- import Data.Hash.MD5
import Data.Pool (Pool)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL

-- import Data.UUID
-- import Data.UUID.V4
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import Config
import Models.User
import Db
import Token
import FromRequest

-- routes :: Pool Connection -> Request
       -- -> (Response -> IO ResponseReceived)
       -- -> IO ResponseReceived
routes pool req respond = do
  case  toMethod req of 
    "POST" -> do
      body <- strictRequestBody req
      case eitherDecode body :: Either String UserIn of
        Left e -> do
          respond (responseLBS status400 [("Content-Type", "text/plain")] $ BL.pack e)
        Right correctlyParsedBody -> do
          exL <- liftIO $ existLogin pool (login correctlyParsedBody)
          case exL of
            False -> do
              c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
              insertUser pool correctlyParsedBody c_date              
              idAdm <- liftIO $ findUserByLogin pool 
                       (login correctlyParsedBody) 
                       (Models.User.password correctlyParsedBody)
              insertImage pool correctlyParsedBody idAdm
              timeStr <- liftIO expirationTime
              let tokenMb =  creatToken idAdm timeStr 
              case tokenMb of
                Nothing -> do
                  respond (responseLBS noContent204 [("Content-Type", "text/plain")] "bad")
                Just token -> do 
                  respond (responseLBS created201 [("Content-Type", "text/plain")] $ encode token)
            True -> do
              respond (responseLBS found302 [("Content-Type", "text/plain")] "user exist")
    
    "GET" -> do
      let token = toToken req
      curtime <- liftIO $ curTimeStr "%Y%m%d%H%M%S"
      case  (testToken token curtime) of
        Left st -> do
          respond (responseLBS st [("Content-Type", "text/plain")] "")
        Right (id, adm) -> do
          userMb <- liftIO $ findUserByID pool id
          case userMb of
            Nothing -> do
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "user not exist")
            Just user -> do 
              respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode user)

    "DELETE" -> do
      let token = toToken req
          id    = toId req
      curtime <- liftIO $ curTimeStr "%Y%m%d%H%M%S"
      case  (testToken token curtime) of
        Left st -> do
          respond (responseLBS st [("Content-Type", "text/plain")] "bad")
        Right (i, adm) -> do
          case adm of      
            True -> do
              deleteUserByID pool id
              respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
            False  -> do
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")



-- так работает 
-- http://localhost:3000/file/?path=images\kita.jpg
  -- get "/file" $ do
    -- path <- param "path" :: ActionM String
    -- file path

    
-- это не работает в адресной строке 
-- http://localhost:3000/images\kita.jpg"

  -- get "/:path" $ do
    -- path <- param "path" :: ActionM String
    -- file path


