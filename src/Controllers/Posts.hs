{-# LANGUAGE OverloadedStrings #-}

module Controllers.Posts
  ( routes
  ) where

import FromRequest
import Services.Db
import Services.Logger
import Services.Token
import Utils

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
    Nothing -> respondWithError hLogger respond status400 "  Invalid or outdated token"
    Just _ ->
      case toId req of
        0 -> getPosts
        _ -> getComments $ toId req
 -- show posts, like
 -- http://localhost:3000/posts/<token>?<filtering and sorting options, page> 
 -- filtering options:
   -- tag=<Integer> tag_in=<[Integer]>, tag_all=<[Integer]>,
   -- created_gt=yyyy-mm-dd, created_lt= , created_at=,
   -- title= , name=, 
   -- category=<Integer>,
   -- text=, find=
 -- sorting option:
   -- order=[fild,...], like order=[photo,date,author,category]
 -- page:
   -- page=
  where
    getPosts = do
      posts <- findAllPosts hDb pool req (limit hDb)
      case posts of
        [] -> respondWithError hLogger respond status404 "  Posts not found"
        xs -> respondWithSuccess respond status200 xs
 -- show comments for post id, like
 -- http://localhost:3000/posts/<token>/<id>[?page=]
    getComments id_post = do
      comments <- findComments hDb pool req (limit hDb) id_post
      case comments of
        [] -> respondWithError hLogger respond status404 "  Commets for this post not found"
        xs -> respondWithSuccess respond status200 xs
