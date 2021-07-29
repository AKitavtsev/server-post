module Servises.Db (Handle(..)) 
    where

import Servises.Config (Config (..))
import qualified Servises.Logger as Logger

import Models.Author
import Models.Category
import Models.Comment
import Models.Draft
import Models.Post
import Models.Tag
import Models.User


import Data.Pool
import Database.PostgreSQL.Simple

data Handle = Handle
    { close              :: Connection -> IO ()
    , newConn            :: Config -> IO Connection
    , runMigrations      :: Logger.Handle
                         -> Connection -> Pool Connection -> FilePath -> IO ()
    , deleteByID         :: Pool Connection -> String -> Integer -> IO ()
    , updateByID         :: Pool Connection -> String -> Integer -> String -> String -> IO ()
    , insertUser         :: Pool Connection -> UserIn -> String-> IO Integer
    , findUserByLogin    :: Pool Connection -> String -> String -> IO (Maybe (Integer, Bool))
    , findUserByID       :: Pool Connection -> Integer -> IO (Maybe UserOut)
    , insertImage        :: Pool Connection -> UserIn ->  Integer -> IO Integer
    , insertImage'       :: Pool Connection -> Integer -> String -> String -> IO ()
    , findImageByID      :: Pool Connection -> Integer -> IO (Maybe (String, String))
    , insertAuthor       :: Pool Connection -> Author -> IO Integer
    , findAuthorByID     :: Pool Connection -> Integer -> IO (Maybe AuthorOut)
    , insertCategory     :: Pool Connection -> Category -> IO Integer
    , findCategoryByID   :: Pool Connection -> Integer -> IO (Maybe Category)
    , updateOwnerCategory:: Pool Connection -> Integer -> String -> IO Integer
    , findSubCat         :: Pool Connection -> Integer -> IO [Integer]
    , insertTag          :: Pool Connection -> Tag     -> IO ()
    , insertTagDraft     :: Pool Connection -> Integer -> Integer -> IO Integer
    , insertPhotoDraft   :: Pool Connection -> Integer -> Integer -> IO Integer
    , findTagByID        :: Pool Connection -> Integer -> IO (Maybe Tag)
    , findTags           :: Pool Connection -> Integer -> Integer -> IO [String]
    -- , checkAvailabilityTags :: Pool Connection -> [Integer] -> IO [Integer]
    -- , checkAvailabilityPhotos :: Pool Connection -> [Integer] -> Integer -> IO [Integer]
    , insertDraft        :: Pool Connection -> DraftIn -> Integer -> String -> IO Integer
    , deleteDraft        :: Pool Connection -> Integer -> Integer -> IO ()
    , updateDraft        :: Pool Connection -> DraftUp -> Integer -> IO (Maybe DraftUp)
    -- , updateDraft        :: Pool Connection -> Integer -> Integer -> String -> String -> IO Integer
    , insertPhoto        :: Pool Connection -> Photo -> IO Integer
    -- , updateMainPhoto    :: Pool Connection -> Integer -> Integer -> Integer -> IO Integer 
    -- , updateOtherPhotos  :: Pool Connection -> Integer -> Integer -> [Integer] -> IO [Integer]    
    , findPhotoByID      :: Pool Connection -> Integer -> IO (Maybe (String, String))
    -- , findPhoto          :: Pool Connection -> Integer -> Integer -> IO (Maybe Integer)
    -- , findDraft          :: Pool Connection -> Integer -> Integer -> IO (Maybe Integer)
    , findDraftByID      :: Pool Connection -> Integer -> IO (Maybe DraftGet)
    -- , takeWholeDraft     :: Pool Connection -> (Integer, Integer) -> IO [String]
    , publishPost        :: Pool Connection -> Integer -> Integer -> IO Integer
    , insertComment      :: Pool Connection -> CommentIn -> Integer -> String -> IO Integer
    , deleteComment      :: Pool Connection -> Integer -> Integer -> IO ()
    , takeAllPosts       :: Pool Connection -> IO [Post]
    }