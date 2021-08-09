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
-- import Servises.Config

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
  logDebug hLogger (show req)
  let token = toToken req
  vt <- validToken hToken token
  case  vt of
    Nothing -> do
       logError hLogger "  Invalid or outdated token"
       respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (id_user, _) -> 
      case toId req of
        0       -> getPosts id_user
        _       -> getComments $ toId req
  where
    getPosts id_user = do  
      posts <- liftIO $ findAllPosts hDb pool req (limit hDb) id_user
      case posts of
        [] -> do
          logInfo hLogger "  Posts not found"
          respond (responseLBS status404 [("Content-Type", "text/plain")] "")
        xs  -> do   
          respond (responseLBS status200 [("Content-Type", "text/plain")]
              $ encode xs)
    
    getComments id_post = do
      comments <- liftIO $ findComments hDb pool req (limit hDb) id_post
      case comments of
        [] -> do
          logInfo hLogger "  Commets for this post not found"
          respond (responseLBS status404 [("Content-Type", "text/plain")] "")
        xs  -> do   
          respond (responseLBS status200 [("Content-Type", "text/plain")]
              $ encode xs)
    

