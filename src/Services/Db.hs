module Services.Db
  ( Handle(..)
  ) where

import Services.Config (Config(..))
import qualified Services.Logger as Logger

import Models.Author
import Models.Category
import Models.Comment
import Models.Draft
import Models.Post
import Models.Tag
import Models.User

import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai (Request(..))

data Handle =
  Handle
    { limit :: Integer
    , close :: Connection -> IO ()
    , newConn :: Config -> IO Connection
    , runMigrations :: Logger.Handle -> Connection -> Pool Connection -> FilePath -> IO ()
    , deleteByID :: Pool Connection -> String -> Integer -> IO ()
    , updateByID :: Pool Connection -> String -> Integer -> String -> IO ()
    , insertUser :: Pool Connection -> RawUser -> String -> IO Integer
    , findUserByLogin :: Pool Connection -> String -> String -> IO (Maybe ( Integer
                                                                          , Bool))
    , findUserByID :: Pool Connection -> Integer -> IO (Maybe ForShowUser)
    , insertImage :: Pool Connection -> RawUser -> Integer -> IO Integer
    , findImageByID :: Pool Connection -> Integer -> IO (Maybe (String, String))
    , insertAuthor :: Pool Connection -> RawAuthor -> IO Integer
    , findAuthorByID :: Pool Connection -> Integer -> IO (Maybe AuthorsDetails)
    , insertCategory :: Pool Connection -> Category -> IO Integer
    , findCategoryByID :: Pool Connection -> Integer -> IO (Maybe Category)
    , updateOwnerCategory :: Pool Connection -> Integer -> String -> IO Integer
    , insertTag :: Pool Connection -> Tag -> IO ()
    , insertTagDraft :: Pool Connection -> Integer -> Integer -> IO Integer
    , insertPhotoDraft :: Pool Connection -> Integer -> Integer -> IO Integer
    , findTagByID :: Pool Connection -> Integer -> IO (Maybe Tag)
    , findTags :: Pool Connection -> Integer -> Integer -> IO [String]
    , insertDraft :: Pool Connection -> RawDraft -> Integer -> String -> IO Integer
    , deleteDraft :: Pool Connection -> Integer -> Integer -> IO ()
    , updateDraft :: Pool Connection -> ForUpdateDraft -> Integer -> IO (Maybe ForUpdateDraft)
    , insertPhoto :: Pool Connection -> Photo -> IO Integer
    , findPhotoByID :: Pool Connection -> Integer -> IO (Maybe (String, String))
    , findDraftByID :: Pool Connection -> Integer -> IO (Maybe ForShowDraft)
    , publishPost :: Pool Connection -> Integer -> Integer -> IO Integer
    , insertComment :: Pool Connection -> RawComment -> Integer -> String -> IO Integer
    , deleteComment :: Pool Connection -> Integer -> Integer -> IO ()
    , findAllPosts :: Pool Connection -> Request -> Integer -> IO [Post]
    , findComments :: Pool Connection -> Request -> Integer -> Integer -> IO [Comment]
    }
