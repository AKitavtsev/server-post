{-# LANGUAGE RankNTypes #-}

module Services.Db
  ( Handle(..)
  ) where

import Models.Author
import Models.Category
import Models.Comment
import Models.Draft
import Models.Post
import Models.Tag
import Models.User

import Network.Wai (Request(..))

data Handle m =
  Handle
    { limit :: Integer
    , deleteByID :: Monad m => String -> Integer -> m ()
    , updateByID :: Monad m => String -> Integer -> String -> m ()
    , insertUser :: Monad m => RawUser -> String -> m Integer
    , findUserByLogin :: Monad m => String -> String -> m (Maybe (Integer, Bool))
    , findUserByID :: Monad m => Integer -> m (Maybe ForShowUser)
    , insertImage :: Monad m => RawUser -> Integer -> m Integer
    , findImageByID :: Monad m => Integer -> m (Maybe (String, String))
    , insertAuthor :: Monad m => RawAuthor -> m Integer
    , findAuthorByID :: Monad m => Integer -> m (Maybe AuthorsDetails)
    , insertCategory :: Monad m => Category -> m Integer
    , findCategoryByID :: Monad m => Integer -> m (Maybe Category)
    , updateParentCategory :: Monad m => Integer -> String -> m Integer
    , insertTag :: Monad m => Tag -> m ()
    , insertTagDraft :: Monad m => Integer -> Integer -> m Integer
    , insertPhotoDraft :: Monad m => Integer -> Integer -> m Integer
    , findTagByID :: Monad m => Integer -> m (Maybe Tag)
    , findTags :: Monad m => Integer -> Integer -> m [String]
    , insertDraft :: Monad m => RawDraft -> Integer -> String -> m Integer
    , deleteDraft :: Monad m => Integer -> Integer -> m ()
    , updateDraft :: Monad m => ForUpdateDraft -> Integer -> m (Maybe ForUpdateDraft)
    , insertPhoto :: Monad m => Photo -> m Integer
    , findPhotoByID :: Monad m => Integer -> m (Maybe (String, String))
    , findDraftByID :: Monad m => Integer -> m (Maybe ForShowDraft)
    , publishPost :: Monad m => Integer -> Integer -> m Integer
    , insertComment :: Monad m => RawComment -> Integer -> String -> m Integer
    , deleteComment :: Monad m => Integer -> Integer -> m ()
    , findAllPosts :: Monad m => Request -> Integer -> m [Post]
    , findComments :: Monad m => Request -> Integer -> Integer -> m [Comment]
    }
