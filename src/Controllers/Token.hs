{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Token
  ( routes
  ) where

import FromRequest
import Servises.Db
import Servises.Logger
import Servises.Token

import Control.Monad.Trans
import Data.Aeson
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

newtype Token =
  Token
    { token :: String
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- getting a token like
--  http://localhost:3000/token/?login=login&password=password
routes ::
     Pool Connection
  -> Servises.Logger.Handle
  -> Servises.Token.Handle
  -> Servises.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  case (,) <$> toParam req "login" <*> toParam req "password" of
    Nothing -> do
      logError hLogger "  Required Parameters \"Login \" and \"Password\""
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (login, password) -> do
      idAdm <- liftIO $ findUserByLogin hDb pool login password
      case idAdm of
        Nothing -> do
          logError hLogger "  Invalid Login/Password"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
        Just (id_, adm) -> do
          token_ <- createToken hToken id_ adm
          respond
            (responseLBS created201 [("Content-Type", "text/plain")] $
             encode (Token token_))
