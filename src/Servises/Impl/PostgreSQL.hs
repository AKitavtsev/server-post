{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Servises.Impl.PostgreSQL 
    ( newHandle
    ) where

import qualified Servises.Config as C
import qualified Servises.Db as SD

import Models.Author
import Models.Category
import Models.Draft
import Models.Tag
import Models.User
import Servises.Impl.PostgreSQL.Migrations

import Database.PostgreSQL.Simple

import Data.Char (toLower)
import Data.Maybe
import Data.Pool (Pool (..), withResource)
import Control.Exception
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import GHC.Int (Int64 (..))

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T

newHandle :: IO SD.Handle
newHandle = do
    return $ SD.Handle
      { SD.close               = close
      , SD.newConn             = newConn
      , SD.runMigrations       = runMigrations
      , SD.deleteByID          = deleteByID
      , SD.updateByID          = updateByID
      , SD.insertUser          = insertUser
      , SD.findUserByLogin     = findUserByLogin
      , SD.findUserByID        = findUserByID
      , SD.insertImage         = insertImage
      , SD.insertImage'        = insertImage'
      , SD.findImageByID       = findImageByID
      , SD.insertAuthor        = insertAuthor
      , SD.findAuthorByID      = findAuthorByID
      , SD.insertCategory      = insertCategory
      , SD.findCategoryByID    = findCategoryByID
      , SD.updateOwnerCategory = updateOwnerCategory
      , SD.insertTag           = insertTag
      , SD.findTagByID         = findTagByID
      , SD.insertDraft         = insertDraft
      , SD.deleteDraft         = deleteDraft
      , SD.updateDraft         = updateDraft
      , SD.insertPhoto         = insertPhoto
      , SD.findPhotoByID       = findPhotoByID
      }

newConn conf = connect defaultConnectInfo
                       { connectUser = C.user conf
                       , connectPassword = C.password conf
                       , connectDatabase = C.name conf
                       }

deleteByID pool model id = do
  case model of
    "user"     -> do liftIO $ execSqlT pool [id] "DELETE FROM users WHERE id=?"
    "author"   -> do liftIO $ execSqlT pool [id] "DELETE FROM authors WHERE id=?"
    "category" -> do liftIO $ execSqlT pool [id] "DELETE FROM categories WHERE id=?"
    "tag"      -> do liftIO $ execSqlT pool [id] "DELETE FROM tags WHERE id=?"
  return ()

updateByID pool model id fild = do
  case model of
    "author"      -> do 
      liftIO $ execSqlT pool [fild, (show id)] 
                   "UPDATE authors SET description =? WHERE id=?"
    "tag"         -> do
      liftIO $ execSqlT pool [fild, (show id)] 
                   "UPDATE tags SET tag=? WHERE id=?"
    "category"    -> do
      liftIO $ execSqlT pool [fild, (show id)]
                   "UPDATE categories SET name=? WHERE id=?"
    "draftPhoto"  -> do
      liftIO $ execSqlT pool [fild, (show id)]
                   "UPDATE drafts SET photo =? WHERE id=?"
    "draftPhotos" -> do
      liftIO $ execSqlT pool [listToSql fild, (show id)]
                   "UPDATE drafts SET photos =? WHERE id=?"
    "drafts     " -> do
      liftIO $ execSqlT pool [fild, (show id)]
                   "UPDATE drafts SET photos =? WHERE id=?"

  return ()
-----------------------------------------------------------------------
insertUser pool (UserIn name surname avatar login password) c_date = do
  res <- liftIO $ fetch pool [name, surname, login, password, c_date, "FALSE"]
        "INSERT INTO users (name, surname, login, password, c_date, admin) VALUES(?,?,?,md5( ?) ,?,?) returning id"
  return $ pass res
  where 
    pass [Only id] = id
    pass _         = 0

findUserByLogin pool login password = do
         res <- liftIO $ fetch pool [login, password] 
                "SELECT id, admin  FROM users WHERE login=? AND password = md5( ?)" ::
                IO [(Integer,  Bool)]
         return $ pass res
         where 
           pass [(id, adm)] = Just (id, adm)
           pass _           = Nothing

findUserByID pool id = do
         res <- liftIO $ fetch pool (Only id) 
                "SELECT name, surname, login, c_date::varchar, admin  FROM users WHERE id=?" ::
                IO [(String, String, String, String, Bool)]
         return $ pass res
         where pass [(n, sn, log, dat, adm)] 
                   = Just (UserOut n sn log dat adm)
               pass _ = Nothing
--------------------------------------------------------------------------------
-- чисто для служебного пользования
insertImage' pool id im t = do
  liftIO $ execSqlT pool [(show id) , im, t]
      "INSERT INTO images (id, image, image_type) VALUES (?,?,?)"
  return ()     
----------------------------------
insertImage pool (UserIn name surname avatar login password) id = do
  case avatar of
    Just (Avatar im t) -> do
      res <- liftIO $ fetch pool [(show id), im, t]
        "INSERT INTO images (id, image, image_type) VALUES (?,?,?) returning id"
      return $ pass res
      where
        pass [Only id] = id
        pass _         = 0
    Nothing  -> return (-1)
  
findImageByID pool id = do
         res <- liftIO $ fetch pool (Only id) 
                "SELECT image, image_type FROM images WHERE id=?"
                :: IO [(String, String)]
         return $ pass res
         where pass [(img, t)] = Just (img, t)
               pass _ = Nothing
--------------------------------------------
insertAuthor pool (Author id description) = do
  res <- liftIO $ fetch pool [show id, T.unpack description]
        "INSERT INTO authors  (id, description) VALUES(?,?) returning id"
  return $ pass res
  where
    pass [Only id] = id
    pass _         = 0

findAuthorByID pool id = do
    res <- liftIO $ fetch pool (Only id) 
           -- "SELECT FROM authors, users  WHERE id=?" ::
            -- IO [(Integer, T.Text)]           
              "SELECT users.name, users.surname, authors.description FROM users, authors WHERE users.id = authors.id AND authors.id = ?;"           
    return $ pass res
      where pass [(name, surname, descr)] = Just (AuthorOut name surname descr)
            pass _ = Nothing
--------------------------------
insertCategory pool (Category name owner_id) = do
  case owner_id of
    Just owner -> do
      res <- liftIO $ fetch pool [name, show owner]
        "INSERT INTO categories (name, id_owner) VALUES(?,?) returning id"
      return $ pass res
    Nothing    -> do  
      res <- liftIO $ fetch pool [name]
        "INSERT INTO categories (name) VALUES(?) returning id"
      return $ pass res 
  where
    pass [Only id] = id
    pass _         = 0

findCategoryByID pool id = do
  res <- liftIO $ fetch pool (Only id)
            "SELECT * FROM categories WHERE id=?"  :: IO [(Integer, String, Maybe Integer)]   
  return $ pass res
  where pass [(id, name, idOw)] = Just (Category name idOw)
        pass _                  = Nothing

updateOwnerCategory pool id owner = do
  case map toLower owner of
    "null" -> do
      res <- liftIO $ fetch pool [(show id)]
                  "UPDATE categories SET id_owner=null WHERE id=? returning id"
      return $ pass res                  
    _      -> do
      res <- liftIO $ fetch pool [owner, (show id)]
                  "UPDATE categories SET id_owner=? WHERE id=? returning id" 
      return $ pass res                  
  where
    pass [Only id] = id
    pass _         = 0
---------------------------------------------------------------------------     
insertTag pool (Tag tag) = do
      liftIO $ execSqlT pool [tag]
        "INSERT INTO tags (tag) VALUES(?)"
      return ()

findTagByID pool id = do
     res <- liftIO $ fetch pool (Only id)
              "SELECT * FROM tags WHERE id=?"  :: IO [(Integer, String)]   
     return $ pass res
         where pass [(id, tag)] = Just (Tag tag)
               pass _                  = Nothing
----------------------------------------------------------------------------------
insertDraft pool (DraftIn id_draft title category tags t_content mainPhoto otherPhotos) 
                  id c_date = do
  res <- liftIO $ fetch pool [ fromMaybe "" title
                             , show id
                             , c_date
                             , show $ fromMaybe 0 category
                             , listToSql $ show $ fromMaybe [] tags
                             , T.unpack $ fromMaybe "" t_content]
        "INSERT INTO drafts (title, author, c_date, category, tags, t_content) VALUES(?,?,?,?,?,?) returning id"
  return $ pass res
  where 
    pass [Only id] = id
    pass _         = 0

deleteDraft pool id author = do
  liftIO $ execSqlT pool [id, author] "DELETE FROM drafts WHERE id=? AND author =?" 
  return ()

updateDraft pool id author content = do
  liftIO $ execSqlT pool [content, (show id), (show author)] "UPDATE drafts SET t_content=? WHERE id=? AND author =?" 
  return ()
  
insertPhoto pool author (Photo im t)  = do
  res <- liftIO $ fetch pool [im, t, show author]
        "INSERT INTO photos (image, image_type, author) VALUES (?,?,?) returning id"
  return $ pass res
  where
        pass [Only id] = id
        pass _         = 0

findPhotoByID pool id = do
         res <- liftIO $ fetch pool (Only id) 
                "SELECT image, image_type FROM photos WHERE id=?"
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
--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
     
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = do
         -- query conn sql args
              resEither <- try (query conn sql args)
              testException resEither
             
testException :: (Either SqlError [r]) ->  IO [r]
testException resEither = do
    case resEither of
        Right val -> return val
        Left ex   -> do
          putStrLn (show ex)
          return []
      
      

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
listToSql :: String -> String
listToSql list = init ('{': (tail list)) ++ "}"