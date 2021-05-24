module Servises.Db (Handle(..)) 
    where

import Servises.Config (Config (..))
import qualified Servises.Logger as Logger
import Models.User

import Data.Pool
import Database.PostgreSQL.Simple

data Handle = Handle
    { close           :: Connection -> IO ()
    , newConn         :: Config -> IO Connection
    , runMigrations   :: Logger.Handle 
                      -> Connection -> Pool Connection -> FilePath -> IO ()
    , insertUser      :: Pool Connection -> UserIn -> String-> IO ()
    , existLogin      :: Pool Connection -> String -> IO Bool
    , findUserByLogin :: Pool Connection -> String -> String -> IO (Maybe (Integer, Bool))
    , findUserByID    :: Pool Connection -> Integer -> IO (Maybe UserOut)
    , deleteUserByID  :: Pool Connection -> Integer -> IO ()
    , insertImage     :: Pool Connection -> UserIn ->  Maybe (Integer, Bool) -> IO ()
    , insertImage'    :: Pool Connection -> Integer -> String -> String -> IO ()
    , findImageByID   :: Pool Connection -> Integer -> IO (Maybe (String, String))    
    }