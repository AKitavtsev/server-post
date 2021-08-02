{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Posts
    where

import FromRequest
import Models.Post
import Servises.Db
import Servises.Logger
import Servises.Token
import Servises.Config

import Control.Monad.Trans
import Data.Aeson
import Data.List (dropWhile)
import Control.Monad (when)
import GHC.Generics
import Network.HTTP.Types.Status
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64


routes pool hLogger hToken hDb req respond = do
  let token = toToken req
  vt <- validToken hToken token
  case  vt of
    Nothing -> do
       logError hLogger "  Invalid or outdated token"
       respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just _ -> do
      posts <- liftIO $ findAllPosts hDb pool req
      case posts of
        [] -> do
          logError hLogger "  Posts not found"
          respond (responseLBS status404 [("Content-Type", "text/plain")] "")
        xs  -> do   
          respond (responseLBS status200 [("Content-Type", "text/plain")]
              $ encode xs)

  

