
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
    , deleteByID :: String -> Integer -> m ()
    , updateByID :: String -> Integer -> String -> m ()
    , insertUser :: RawUser -> String -> m Integer
    , findUserByLogin :: String -> String -> m (Maybe (Integer, Bool))
    , findUserByID :: Integer -> m (Maybe ForShowUser)
    , insertImage :: RawUser -> Integer -> m Integer
    , findImageByID :: Integer -> m (Maybe (String, String))
    , insertAuthor :: RawAuthor -> m Integer
    , findAuthorByID :: Integer -> m (Maybe AuthorsDetails)
    , insertCategory :: Category -> m Integer
    , findCategoryByID :: Integer -> m (Maybe Category)
    , updateParentCategory :: Integer -> String -> m Integer
    , insertTag :: Tag -> m ()
    , insertTagDraft :: Integer -> Integer -> m Integer
    , insertPhotoDraft :: Integer -> Integer -> m Integer
    , findTagByID :: Integer -> m (Maybe Tag)
    , findTags :: Integer -> Integer -> m [String]
    , insertDraft :: RawDraft -> Integer -> String -> m Integer
    , deleteDraft :: Integer -> Integer -> m ()
    , updateDraft :: ForUpdateDraft -> Integer -> m (Maybe ForUpdateDraft)
    , insertPhoto :: Photo -> m Integer
    , findPhotoByID :: Integer -> m (Maybe (String, String))
    , findDraftByID :: Integer -> m (Maybe ForShowDraft)
    , publishPost :: Integer -> Integer -> m Integer
    , insertComment :: RawComment -> Integer -> String -> m Integer
    , deleteComment :: Integer -> Integer -> m ()
    , findAllPosts :: Request -> Integer -> m [Post]
    , findComments :: Request -> Integer -> Integer -> m [Comment]
    }
