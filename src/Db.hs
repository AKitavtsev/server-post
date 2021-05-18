{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Db (newConn
          -- , fetch
          -- , execSqlT
          , insertUser
          , existLogin
          , findUserByLogin
          , findUserByID
          , deleteUserByID
          , insertImage
          , insertImage'
          , findImageByID
          ) where

import qualified Config as C
import Models.User

-- import Token (curTimeStr)

-- import Web.Scotty.Internal.Types (ActionT)
-- import GHC.Generics (Generic)
-- import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
-- import Data.Pool(Pool, createPool, withResource)
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Encoding as TL
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Text as T
import GHC.Int
import Data.Pool
import Control.Monad.Trans
import Control.Monad (when)

import qualified Data.ByteString.Lazy.Char8 as LC
-- import qualified Data.ByteString as BS

-- DbConfig contains info needed to connect to MySQL server
-- data DbConfig = DbConfig {
     -- dbName :: String,
     -- dbUser :: String,
     -- dbPassword :: String
     -- }
     -- deriving (Show, Generic)

-- The function knows how to create new DB connection
-- It is needed to use with resource pool
newConn :: C.DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                       { connectUser = C.user conf
                       , connectPassword = C.password conf
                       , connectDatabase = C.name conf
                       }

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

-- No arguments -- just pure sql
-- fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
-- fetchSimple pool sql = withResource pool retrieve
       -- where retrieve conn = query_ conn sql

-- Update database
-- execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
-- execSql pool args sql = withResource pool ins
       -- where ins conn = execute conn sql args

-------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- Transactions.
--
-- Accepts arguments
-- fetchT :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
-- fetchT pool args sql = withResource pool retrieve
      -- where retrieve conn = withTransaction conn $ query conn sql args

-- No arguments -- just pure sql
-- fetchSimpleT :: FromRow r => Pool Connection -> Query -> IO [r]
-- fetchSimpleT pool sql = withResource pool retrieve
       -- where retrieve conn = withTransaction conn $ query_ conn sql

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
       where ins conn = withTransaction conn $ execute conn sql args

--------------------------------------------------------------------------------

insertUser :: Pool Connection -> UserIn -> String-> IO ()
insertUser pool (UserIn name surname avatar login password) c_date = do
  liftIO $ execSqlT pool [name, surname, login, password, c_date, "FALSE"]
        "INSERT INTO users (name, surname, login, password, c_date, admin) VALUES(?,?,?,md5( ?) ,?,?)"

  -- case avatar of
    -- Nothing -> 
      -- liftIO $ execSqlT pool [name, surname, login, password, c_date, "FALSE"]
        -- "INSERT INTO users (name, surname, login, password, c_date, admin) VALUES(?,?,?,md5( ?) ,?,?)"
    -- Just (Avatar im t) -> do
      -- liftIO $ execSqlT pool [im, t]
        -- "INSERT INTO images (image, image_type) VALUES(?,?)"
      -- image_id <- findLastImage pool                     
      -- liftIO $ execSqlT pool [name, surname, (show image_id), login, password, c_date, "FALSE"]
        -- "INSERT INTO users (name, surname, avatar, login, password, c_date, admin) VALUES(?,?,?,?,md5( ?) ,?,?)"    
  return ()

existLogin :: Pool Connection -> String -> IO Bool
existLogin pool login = do
    res <- liftIO $ fetch pool (Only login) 
           "SELECT user_id, password FROM users WHERE login=?" :: IO [(Integer, String)]
    return $ pass res
         where pass [(id, passw)] = True
               pass _ = False 

findUserByLogin :: Pool Connection -> String -> String -> IO (Maybe (Integer, Bool))
findUserByLogin pool login password = do
         res <- liftIO $ fetch pool [login, password] 
                "SELECT user_id, admin  FROM users WHERE login=? AND password = md5( ?)" ::
                IO [(Integer,  Bool)]
         return $ pass res
         where 
           pass [(id, adm)] = Just (id, adm)
           pass _           = Nothing

findUserByID :: Pool Connection -> Integer -> IO (Maybe UserOut)
findUserByID pool id = do
         res <- liftIO $ fetch pool (Only id) 
                "SELECT user_id, name, surname, login, c_date::varchar, admin  FROM users WHERE user_id=?" ::
                IO [(Integer, String, String, String, String, Bool)]
         return $ pass res
         where pass [(id, n, sn, log, dat, adm)] = Just (UserOut id n sn log dat adm)
               pass _ = Nothing

deleteUserByID :: Pool Connection -> Integer -> IO ()
deleteUserByID pool id = do
     liftIO $ execSqlT pool [id] "DELETE FROM users WHERE user_id=?"
     return ()
--------------------------------------------------------------------------------
-- чисто для служебного пользования
insertImage' :: Pool Connection -> Integer -> String -> String -> IO ()
insertImage' pool id im t = do
  liftIO $ execSqlT pool [(show id) , im, t]
      "INSERT INTO images (image_id, image, image_type) VALUES (?,?,?)"
  return ()     

insertImage :: Pool Connection -> UserIn ->  Maybe (Integer, Bool) -> IO ()
insertImage pool (UserIn name surname avatar login password) (Just (id, adm)) = do
  when (not (avatar == Nothing)) $ do
    case avatar of
      Just (Avatar im t) -> liftIO $ execSqlT pool [(show id), im, t]
        "INSERT INTO images (image_id, image, image_type) VALUES (?,?,?)"
    return ()
  return ()
       
findImageByID:: Pool Connection -> Integer -> IO (Maybe (String, String))
findImageByID pool id = do
         res <- liftIO $ fetch pool (Only id) 
                "SELECT image, image_type FROM images WHERE image_id=?"
                :: IO [(String, String)]
         return $ pass res
         where pass [(img, t)] = Just (img, t)
               pass _ = Nothing

-- listArticles :: Pool Connection -> IO [Article]
-- listArticles pool = do
     -- res <- fetchSimple pool "SELECT * FROM article ORDER BY id DESC" :: IO [(Integer, TL.Text, TL.Text)]
     -- return $ map (\(id, title, bodyText) -> Article id title bodyText) res
   
-- findArticle :: Pool Connection -> TL.Text -> IO (Maybe Article)
-- findArticle pool id = do
     -- res <- fetch pool (Only id) "SELECT * FROM article WHERE id=?" :: IO [(Integer, TL.Text, TL.Text)]
     -- return $ oneArticle res
     -- where oneArticle ((id, title, bodyText) : _) = Just $ Article id title bodyText
           -- oneArticle _ = Nothing


-- insertArticle :: Pool Connection -> Maybe Article -> ActionT TL.Text IO ()
-- insertArticle pool Nothing = return ()
-- insertArticle pool (Just (Article id title bodyText)) = do
     -- liftIO $ execSqlT pool [title, bodyText]
                            -- "INSERT INTO article(title, bodyText) VALUES(?,?)"
     -- return ()

-- updateArticle :: Pool Connection -> Maybe Article -> ActionT TL.Text IO ()
-- updateArticle pool Nothing = return ()
-- updateArticle pool (Just (Article id title bodyText)) = do
     -- liftIO $ execSqlT pool [title, bodyText, (TL.decodeUtf8 $ BL.pack $ show id)]
                            -- "UPDATE article SET title=?, bodyText=? WHERE id=?"
     -- return ()

-- deleteArticle :: Pool Connection -> TL.Text -> ActionT TL.Text IO ()
-- deleteArticle pool id = do
     -- liftIO $ execSqlT pool [id] "DELETE FROM article WHERE id=?"
     -- return ()
