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

data Handle =
  Handle
    { limit :: Integer
    , deleteByID :: String -> Integer -> IO ()
    , updateByID :: String -> Integer -> String -> IO ()
    , insertUser :: RawUser -> String -> IO Integer
    , findUserByLogin :: String -> String -> IO (Maybe (Integer, Bool))
    , findUserByID :: Integer -> IO (Maybe ForShowUser)
    , insertImage :: RawUser -> Integer -> IO Integer
    , findImageByID :: Integer -> IO (Maybe (String, String))
    , insertAuthor :: RawAuthor -> IO Integer
    , findAuthorByID :: Integer -> IO (Maybe AuthorsDetails)
    , insertCategory :: Category -> IO Integer
    , findCategoryByID :: Integer -> IO (Maybe Category)
    , updateOwnerCategory :: Integer -> String -> IO Integer
    , insertTag :: Tag -> IO ()
    , insertTagDraft :: Integer -> Integer -> IO Integer
    , insertPhotoDraft :: Integer -> Integer -> IO Integer
    , findTagByID :: Integer -> IO (Maybe Tag)
    , findTags :: Integer -> Integer -> IO [String]
    , insertDraft :: RawDraft -> Integer -> String -> IO Integer
    , deleteDraft :: Integer -> Integer -> IO ()
    , updateDraft :: ForUpdateDraft -> Integer -> IO (Maybe ForUpdateDraft)
    , insertPhoto :: Photo -> IO Integer
    , findPhotoByID :: Integer -> IO (Maybe (String, String))
    , findDraftByID :: Integer -> IO (Maybe ForShowDraft)
    , publishPost :: Integer -> Integer -> IO Integer
    , insertComment :: RawComment -> Integer -> String -> IO Integer
    , deleteComment :: Integer -> Integer -> IO ()
    , findAllPosts :: Request -> Integer -> IO [Post]
    , findComments :: Request -> Integer -> Integer -> IO [Comment]
    }
