{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Users 
    where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
import Data.Pool (Pool)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time

import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import Controllers.Token (Token (..))
import FromRequest
import Models.User
import Servises.Config
import Servises.Token
import Servises.Db


-- routes :: Pool Connection -> Request
       -- -> (Response -> IO ResponseReceived)
       -- -> IO ResponseReceived
routes pool hLogger hToken hDb req respond = do
  case  toMethod req of
    "POST" -> do
      body <- strictRequestBody req
      case eitherDecode body :: Either String UserIn of
        Left e -> do
          respond (responseLBS status400 [("Content-Type", "text/plain")] $ BL.pack e)
        Right correctlyParsedBody -> do
          exL <- liftIO $ existLogin hDb pool (login correctlyParsedBody)
          case exL of
            False -> do
              c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
              insertUser hDb pool correctlyParsedBody c_date              
              idAdm <- liftIO $ findUserByLogin hDb pool 
                       (login correctlyParsedBody) 
                       (Models.User.password correctlyParsedBody)
              insertImage hDb pool correctlyParsedBody idAdm
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
          userMb <- liftIO $ findUserByID hDb pool id
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
              deleteUserByID hDb pool id
              respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
            False  -> do
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")

curTimeStr :: String -> IO String
curTimeStr form = do
    utc <- Time.getCurrentTime
    return (Time.formatTime Time.defaultTimeLocale form utc)


