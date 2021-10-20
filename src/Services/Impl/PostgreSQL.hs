{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL
  ( newHandle
  ) where

import qualified Services.Db as SD

import Config
import Services.Impl.PostgreSQL.Author
import Services.Impl.PostgreSQL.Category
import Services.Impl.PostgreSQL.Comment
import Services.Impl.PostgreSQL.Draft
import Services.Impl.PostgreSQL.Internal
import Services.Impl.PostgreSQL.Post
import Services.Impl.PostgreSQL.Tag
import Services.Impl.PostgreSQL.User

import Data.Pool
import Database.PostgreSQL.Simple
import GHC.Int (Int64(..))

newHandle :: Config -> Pool Connection -> IO (SD.Handle IO)
newHandle config pool = do
  return $
    SD.Handle
      { SD.limit = limit config
      , SD.deleteByID = deleteByID
      , SD.updateByID = updateByID
      , SD.insertUser = insertUser pool
      , SD.findUserByLogin = findUserByLogin pool
      , SD.findUserByID = findUserByID hostPort pool 
      , SD.insertImage = insertImage pool
      , SD.findImageByID = findImageByID pool
      , SD.insertAuthor = insertAuthor pool
      , SD.findAuthorByID = findAuthorByID hostPort pool
      , SD.insertCategory = insertCategory pool
      , SD.findCategoryByID = findCategoryByID pool
      , SD.updateOwnerCategory = updateOwnerCategory pool
      , SD.insertTag = insertTag pool
      , SD.insertTagDraft = insertTagDraft pool
      , SD.insertPhotoDraft = insertPhotoDraft pool
      , SD.findTags = findTags pool
      , SD.insertDraft = insertDraft pool
      , SD.deleteDraft = deleteDraft pool
      , SD.updateDraft = updateDraft pool
      , SD.insertPhoto = insertPhoto pool
      , SD.findTagByID = findTagByID pool
      , SD.findPhotoByID = findPhotoByID pool
      , SD.findDraftByID = findDraftByID hostPort pool
      , SD.publishPost = publishPost pool
      , SD.insertComment = insertComment pool
      , SD.deleteComment = deleteComment pool
      , SD.findAllPosts = findAllPosts hostPort pool
      , SD.findComments = findComments hostPort pool
      }
  where
    deleteByID model id_ = do
      _ <-
        case model of
          "user" -> execSqlT pool [id_] "DELETE FROM user_ WHERE user_id=?"
          "author" -> execSqlT pool [id_] "DELETE FROM author WHERE user_id=?"
          "category" ->
            execSqlT pool [id_] "DELETE FROM category WHERE category_id=?"
          "tag" -> execSqlT pool [id_] "DELETE FROM tag WHERE tag_id=?"
          "comment" -> execSqlT pool [id_] "DELETE FROM comment WHERE id_=?"
          "tag_draft" ->
            execSqlT pool [id_] "DELETE FROM  tag_draft WHERE draft_id=?"
          "photo_draft" ->
            execSqlT pool [id_] "DELETE FROM  photo_draft WHERE draft_id=?"
          _ -> return (0 :: Int64)
      return ()
    updateByID model id_ value = do
      _ <-
        case model of
          "author" ->
            execSqlT
              pool
              [value, show id_]
              "UPDATE author SET description =? WHERE user_id=?"
          "tag" ->
            execSqlT
              pool
              [value, show id_]
              "UPDATE tag SET tag=? WHERE tag_id=?"
          "category" ->
            execSqlT
              pool
              [value, show id_]
              "UPDATE category SET category_name=? WHERE category_id=?"
          _ -> return (0 :: Int64)
      return ()
 
    hostPort = host config ++ ":" ++ show (port config)
 