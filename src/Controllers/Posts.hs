{-# LANGUAGE OverloadedStrings #-}

module Controllers.Posts
  ( routes
  ) where

import FromRequest
import Services.Db
import Services.Logger
import Services.Token

import Data.Aeson
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal
import Network.HTTP.Types.Status
import Network.Wai

routes ::
     Pool Connection
  -> Services.Logger.Handle
  -> Services.Token.Handle
  -> Services.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  let token = toToken req
  vt <- validToken hToken token
  case vt of
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just _ ->
      case toId req of
        0 -> getPosts
        _ -> getComments $ toId req
 -- show posts, like
 -- http://localhost:3000/posts/<token>?<filtering and sorting options> 
 -- filtering options:
   -- tag=<Integer> tag_in=<[Integer]>, tag_all=<[Integer]>,
   -- created_gt=yyyy-mm-dd, created_lt= , created_at=,
   -- title= , name=, 
   -- category=<Integer>,
   -- text=, find=
 -- sorting option:
   -- order=[fild,...], like order=[photo,date,author,category]
 -- or
 -- http://localhost:3000/posts/<token>?page=<номер страницы пагинации>
  where
    getPosts = do
      posts <- findAllPosts hDb pool req (limit hDb)
      case posts of
        [] -> do
          logInfo hLogger "  Posts not found"
          respond (responseLBS status404 [("Content-Type", "text/plain")] "")
        xs -> do
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $ encode xs)
 -- show comments for post id, like
 -- http://localhost:3000/posts/<token>/<id>
 -- or
 -- http://localhost:3000/posts/<token>/<id>?page=<номер страницы пагинации>
    getComments id_post = do
      comments <- findComments hDb pool req (limit hDb) id_post
      case comments of
        [] -> do
          logInfo hLogger "  Commets for this post not found"
          respond (responseLBS status404 [("Content-Type", "text/plain")] "")
        xs -> do
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $ encode xs)
