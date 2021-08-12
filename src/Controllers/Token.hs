{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Token where

import FromRequest
import Servises.Db
import Servises.Logger
import Servises.Token

import Control.Monad.Trans
import Data.Aeson
import Data.Char (isDigit)
import Data.Hash.MD5
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

data Token = Token {token :: String}
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- getting a token like
--  http://localhost:3000/token/?login=login&password=password
routes pool hLogger hToken hDb req respond = do
    case (\x y -> (x, y)) <$> toParam req "login" <*> toParam req "password" of
        Nothing -> do
            logError hLogger "  Required Parameters \"Login \" and \"Password\""
            respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Just (login, password) -> do
            idAdm <- liftIO $ findUserByLogin hDb pool login password
            case idAdm of
                Nothing -> do
                    logError hLogger "  Invalid Login/Password"
                    respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
                Just (id, adm) -> do
                    token <- (createToken hToken) id adm
                    respond
                        ( responseLBS created201 [("Content-Type", "text/plain")] $
                            encode (Token token)
                        )
