{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL
  ( newHandle
  ) where

import qualified Config as C
import qualified Services.Db as SD

import Services.Impl.PostgreSQL.Internal
import Services.Impl.PostgreSQL.User
import Services.Impl.PostgreSQL.Author
import Services.Impl.PostgreSQL.Category
import Services.Impl.PostgreSQL.Comment
import Services.Impl.PostgreSQL.Draft
import Services.Impl.PostgreSQL.Post
import Services.Impl.PostgreSQL.Tag

import Database.PostgreSQL.Simple
import GHC.Int (Int64(..))

newHandle :: C.Config -> IO SD.Handle
newHandle config = do
  return $
    SD.Handle
      { SD.limit = C.limit config
      , SD.close = close
      , SD.newConn = newConn
      , SD.deleteByID = deleteByID
      , SD.updateByID = updateByID
      , SD.insertUser = insertUser
      , SD.findUserByLogin = findUserByLogin
      , SD.findUserByID = findUserByID hostPort
      , SD.insertImage = insertImage
      , SD.findImageByID = findImageByID
      , SD.insertAuthor = insertAuthor
      , SD.findAuthorByID = findAuthorByID hostPort
      , SD.insertCategory = insertCategory
      , SD.findCategoryByID = findCategoryByID
      , SD.updateOwnerCategory = updateOwnerCategory
      , SD.insertTag = insertTag
      , SD.insertTagDraft = insertTagDraft
      , SD.insertPhotoDraft = insertPhotoDraft
      , SD.findTags = findTags
      , SD.insertDraft = insertDraft
      , SD.deleteDraft = deleteDraft
      , SD.updateDraft = updateDraft
      , SD.insertPhoto = insertPhoto
      , SD.findTagByID = findTagByID
      , SD.findPhotoByID = findPhotoByID
      , SD.findDraftByID = findDraftByID hostPort
      , SD.publishPost = publishPost
      , SD.insertComment = insertComment
      , SD.deleteComment = deleteComment
      , SD.findAllPosts = findAllPosts hostPort
      , SD.findComments = findComments hostPort
      }
  where
    newConn conf =
      connect
        defaultConnectInfo
          { connectUser = C.user conf
          , connectPassword = C.password conf
          , connectDatabase = C.name conf
          }
    deleteByID pool model id_ = do
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
    updateByID pool model id_ value = do
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
 
    hostPort = C.host config ++ ":" ++ show (C.port config)
 