{-# LANGUAGE OverloadedStrings #-}

module Controllers.Posts
  ( routes
  ) where

import FromRequest
import Services.Db
import Services.Logger
import Services.Token
import Utils

import Network.HTTP.Types.Status
import Network.Wai

routes :: Monad m =>
     Services.Logger.Handle m
  -> Services.Token.Handle m
  -> Services.Db.Handle m
  -> Request
  -> (Response -> m b)
  -> m b
routes hLogger hToken hDb req respond = do
  let token = toToken req
  vt <- validToken hToken token
  case vt of
    Nothing ->
      respondWithError hLogger respond status401 "  Invalid or outdated token"
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
      posts <- findAllPosts hDb req (limit hDb)
      case posts of
        [] -> respondWithError hLogger respond status404 "  Posts not found"
        xs -> respondWithSuccess respond status200 xs
 -- show comments for post id, like
 -- http://localhost:3000/posts/<token>/<id>[?page=]
    getComments id_post = do
      comments <- findComments hDb req (limit hDb) id_post
      case comments of
        [] ->
          respondWithError
            hLogger
            respond
            status404
            "  Comments for this post not found"
        xs -> respondWithSuccess respond status200 xs
