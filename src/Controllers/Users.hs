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
import qualified Data.Time as Time

-- import Data.UUID
-- import Data.UUID.V4
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import Servises.Token
import Servises.Config
import Models.User
import Db
import Controllers.Token (Token (..))
import FromRequest

-- routes :: Pool Connection -> Request
       -- -> (Response -> IO ResponseReceived)
       -- -> IO ResponseReceived
routes pool hLogger hToken req respond = do
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
              case idAdm of 
                Just (id, adm) -> do
                  token <- (createToken hToken) id adm
                  respond (responseLBS created201 
                           [("Content-Type", "text/plain")] $ encode (Token token))
                Nothing -> respond (responseLBS noContent204 [("Content-Type", "text/plain")] "bad")
            True -> do
              respond (responseLBS found302 [("Content-Type", "text/plain")] "user exist")
    
    "GET" -> do
      let token = toToken req
      vt <- validToken hToken token
      case vt of
        Just (id, _) -> do
          userMb <- liftIO $ findUserByID pool id
          case userMb of
            Nothing -> do
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "user not exist")
            Just user -> do 
              respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode user)
        Nothing -> do
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")        

    "DELETE" -> do
      let token = toToken req
          id    = toId req
      vt <- validToken hToken token
      case  vt of
        Nothing -> do
          respond (responseLBS status400 [("Content-Type", "text/plain")] "bad")
        Just (id, adm) -> do
          case adm of      
            True -> do
              deleteUserByID pool id
              respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
            False  -> do
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")

curTimeStr :: String -> IO String
curTimeStr form = do
    utc <- Time.getCurrentTime
    return (Time.formatTime Time.defaultTimeLocale form utc)


