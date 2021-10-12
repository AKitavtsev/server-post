{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Token
  ( routes
  ) where

import FromRequest
import Services.Db
import Services.Logger
import Services.Token
import Utils

import Data.Aeson
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
     Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes  hLogger hToken hDb req respond = do
  case (,) <$> toParam req "login" <*> toParam req "password" of
    Nothing -> respondWithError hLogger respond status400
                 "  Required Parameters \"Login \" and \"Password\""
    Just (login, password) -> do
      idAdm <- findUserByLogin hDb  login password
      case idAdm of
        Nothing -> respondWithError hLogger respond status404
                     ("  Invalid Login/Password: " ++ login ++ "/" ++ password)
        Just (id_, adm) -> do
          token_ <- createToken hToken id_ adm
          respondWithSuccess respond status201 (Token token_)
