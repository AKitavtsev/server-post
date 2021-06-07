module Servises.Db (Handle(..)) 
    where

import Servises.Config (Config (..))
import qualified Servises.Logger as Logger

import Models.Author
import Models.Category
import Models.Draft
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
    , updateByID         :: Pool Connection -> String -> Integer -> String -> IO ()
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
    , insertTag          :: Pool Connection -> Tag     -> IO ()
    , findTagByID        :: Pool Connection -> Integer -> IO (Maybe Tag)
    , insertDraft        :: Pool Connection -> DraftIn -> Integer -> String -> IO Integer
    , deleteDraft        :: Pool Connection -> Integer -> Integer -> IO ()
    , updateDraft        :: Pool Connection -> Integer -> Integer -> String -> IO ()
    , insertPhoto        :: Pool Connection -> Integer -> Photo -> IO Integer   
    , findPhotoByID      :: Pool Connection -> Integer -> IO (Maybe (String, String))
    }