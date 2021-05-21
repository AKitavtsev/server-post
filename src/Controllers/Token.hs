{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Token
    where

import Servises.Token
import FromRequest
import Db

-- import Control.Monad.Trans
-- import Database.PostgreSQL.Simple
-- import Data.Pool (Pool)
-- import Network.HTTP.Types.Status

import Control.Monad.Trans
import Data.Aeson
import Data.Char (isDigit)
import Data.Hash.MD5
import Data.Pool (Pool)
import Data.Time.Clock
import GHC.Generics
-- import Network.HTTP.Types.Status
import Network.HTTP.Types
import Network.Wai


data Token = Token {token :: String}  
             deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- routes :: Pool Connection -> Request
       -- -> (Response -> IO ResponseReceived)
       -- -> IO ResponseReceived
routes pool hLogger hToken req respond = do
  case  toParam req "login" of
    Nothing -> do
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just login -> do
      case toParam req "password" of
        Nothing -> do 
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Just password -> do
          idAdm   <- liftIO $ findUserByLogin pool login password    
          case idAdm of
            Nothing -> do
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
            Just (id, adm) -> do
              token <- (createToken hToken) id adm
              respond (responseLBS created201 [("Content-Type", "text/plain")]
                      $ encode (Token token))
