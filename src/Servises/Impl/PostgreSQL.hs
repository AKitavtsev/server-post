{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servises.Impl.PostgreSQL 
    ( newHandle
    ) where

import qualified Servises.Config as C
import qualified Servises.Db as SD

import Models.Author
import Models.Category
import Models.Comment
import Models.Draft
import Models.Post
import Models.Tag
import Models.User
import Servises.Impl.PostgreSQL.Migrations

import Database.PostgreSQL.Simple

import Control.Exception
import Control.Monad.Trans (liftIO)
import Control.Monad (when, forM_)
import Data.Char (toLower)
import Data.Maybe
import Data.Pool (Pool (..), withResource)
import Data.String (fromString)
import GHC.Int (Int64 (..))

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T

newHandle :: IO SD.Handle
newHandle = do
    return $ SD.Handle
      { SD.close                   = close
      , SD.newConn                 = newConn
      , SD.runMigrations           = runMigrations
      , SD.deleteByID              = deleteByID
      , SD.updateByID              = updateByID
      , SD.insertUser              = insertUser
      , SD.findUserByLogin         = findUserByLogin
      , SD.findUserByID            = findUserByID
      , SD.insertImage             = insertImage
      , SD.insertImage'            = insertImage'
      , SD.findImageByID           = findImageByID
      , SD.insertAuthor            = insertAuthor
      , SD.findAuthorByID          = findAuthorByID
      , SD.insertCategory          = insertCategory
      , SD.findCategoryByID        = findCategoryByID
      , SD.updateOwnerCategory     = updateOwnerCategory
      , SD.findSubCat              = findSubCat
      , SD.insertTag               = insertTag
      , SD.insertTagDraft          = insertTagDraft
      , SD.insertPhotoDraft        = insertPhotoDraft
      , SD.findTags                = findTags
      -- , SD.checkAvailabilityTags   = checkAvailabilityTags
      -- , SD.checkAvailabilityPhotos = checkAvailabilityPhotos
      , SD.insertDraft             = insertDraft
      , SD.deleteDraft             = deleteDraft
      , SD.updateDraft             = updateDraft
      , SD.insertPhoto             = insertPhoto
      -- , SD.updateMainPhoto         = updateMainPhoto
      -- , SD.updateOtherPhotos       = updateOtherPhotos
      , SD.findTagByID             = findTagByID 
      , SD.findPhotoByID           = findPhotoByID
      -- , SD.findPhoto               = findPhoto
      -- , SD.findDraft               = findDraft
      , SD.findDraftByID           = findDraftByID
      -- , SD.takeWholeDraft          = takeWholeDraft
      , SD.publishPost             = publishPost
      , SD.insertComment           = insertComment
      , SD.deleteComment           = deleteComment
      , SD.takeAllPosts            = takeAllPosts
      }

newConn conf = connect defaultConnectInfo
                       { connectUser = C.user conf
                       , connectPassword = C.password conf
                       , connectDatabase = C.name conf
                       }
-- q :: Query
-- q = fromString ("select" ++ " ?")

deleteByID pool model id = do
  -- let q = fromString ("DELETE FROM " ++ model ++ " WHERE id=?") :: Query
  -- liftIO $ execSqlT pool [id] $ fromString ("DELETE FROM " ++ model ++ "s WHERE id=?")
  case model of
    "user"        -> liftIO $ execSqlT pool [id]
                            "DELETE FROM user_ WHERE user_id=?"
    "author"      -> liftIO $ execSqlT pool [id] 
                            "DELETE FROM author WHERE user_id=?"
    "category"    -> liftIO $ execSqlT pool [id]
                         "DELETE FROM category WHERE category_id=?"
    "tag"         -> liftIO $ execSqlT pool [id] "DELETE FROM tag WHERE tag_id=?"
    "comment"     -> liftIO $ execSqlT pool [id] "DELETE FROM comment WHERE id=?"
    "tag_draft"   -> liftIO $ execSqlT pool [id] "DELETE FROM  tag_draft WHERE draft_id=?"
    "photo_draft" -> liftIO $ execSqlT pool [id] "DELETE FROM  photo_draft WHERE draft_id=?"
  return ()

updateByID pool model id fild value = do
  -- liftIO $ execSqlT pool [value, (show id)] 
         -- $ fromString ("UPDATE " ++ model ++ 
                       -- " SET " ++ fild ++ " =? WHERE id=?")
                       
  case model of
    "author"      -> do 
      liftIO $ execSqlT pool [value, (show id)] 
             "UPDATE author SET description =? WHERE user_id=?"
    "tag"         -> do
      liftIO $ execSqlT pool [value, (show id)] 
             "UPDATE tag SET tag=? WHERE tag_id=?"
    "category"    -> do
      liftIO $ execSqlT pool [value, (show id)]
                   "UPDATE category SET category_name=? WHERE category_id=?"
    -- "draftPhoto"  -> do
      -- liftIO $ execSqlT pool [fild, (show id)]
                   -- "UPDATE drafts SET photo =? WHERE id=?"
    -- "draftPhotos" -> do
      -- liftIO $ execSqlT pool [listToSql fild, (show id)]
                   -- "UPDATE drafts SET photos =? WHERE id=?"
  return ()
--User---------------------------------------------------------------------
insertUser pool (UserIn name surname avatar login password) c_date = do
  res <- liftIO $ fetch pool [name, surname, login, password, c_date, "FALSE"]
        "INSERT INTO user_ (user_name, surname, login, password, user_date, admin) VALUES(?,?,?,md5( ?) ,?,?) returning user_id"
  return $ pass res
  where 
    pass [Only id] = id
    pass _         = 0

findUserByLogin pool login password = do
         res <- liftIO $ fetch pool [login, password] 
                "SELECT user_id, admin  FROM user_ WHERE login=? AND password = md5( ?)" ::
                IO [(Integer,  Bool)]
         return $ pass res
         where 
           pass [(id, adm)] = Just (id, adm)
           pass _           = Nothing

findUserByID pool id = do
         res <- liftIO $ fetch pool (Only id) 
                "SELECT user_name, surname, login, user_date::varchar, admin  FROM user_ WHERE user_id=?" ::
                IO [(String, String, String, String, Bool)]
         return $ pass res
         where pass [(n, sn, log, dat, adm)] 
                      = Just (UserOut n sn 
                           ("http://localhost:3000/image/" ++ (show id))
                           log dat adm)
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
        "INSERT INTO image (user_id, image, image_type) VALUES (?,?,?) returning user_id"
      return $ pass res
      where
        pass [Only id] = id
        pass _         = 0
    Nothing  -> return (-1)
  
findImageByID pool id = do
         res <- liftIO $ fetch pool (Only id) 
                "SELECT image, image_type FROM image WHERE user_id=?"
                :: IO [(String, String)]
         return $ pass res
         where pass [(img, t)] = Just (img, t)
               pass _ = Nothing
--Author------------------------------------------
insertAuthor pool (Author id description) = do
  res <- liftIO $ fetch pool [show id, T.unpack description]
        "INSERT INTO author  (user_id, description) VALUES(?,?) returning user_id"
  return $ pass res
  where
    pass [Only id] = id
    pass _         = 0

findAuthorByID pool id = do
    res <- liftIO $ fetch pool (Only id) 
              "SELECT user_name, surname, description FROM user_ INNER JOIN author USING(user_id) WHERE user_.user_id = ?;"           
    return $ pass res
      where pass [(name, surname, descr)] = Just (AuthorOut name surname descr)
            pass _ = Nothing
--Category------------------------------
insertCategory pool (Category name owner_id) = do
  case owner_id of
    Just owner -> do
      res <- liftIO $ fetch pool [name, show owner]
        "INSERT INTO category (category_name, owner_id) VALUES(?,?) returning category_id"
      return $ pass res
    Nothing    -> do  
      res <- liftIO $ fetch pool [name]
        "INSERT INTO category (category_name) VALUES(?) returning category_id"
      return $ pass res 
  where
    pass [Only id] = id
    pass _         = 0

findCategoryByID pool id = do
  res <- liftIO $ fetch pool (Only id)
            "SELECT * FROM category WHERE category_id=?"  :: IO [(Integer, String, Maybe Integer)]   
  return $ pass res
  where pass [(id, name, idOw)] = Just (Category name idOw)
        pass _                  = Nothing

updateOwnerCategory pool id owner = do
  case map toLower owner of
    "null" -> do
      res <- liftIO $ fetch pool [(show id)]
                  "UPDATE category SET owner_id=null WHERE category_id=? returning category_id"
      return $ pass res                  
    _      -> do
      res <- liftIO $ fetch pool [owner, (show id)]
                  "UPDATE category SET owner_id=? WHERE category_id=? returning category_id" 
      return $ pass res                  
  where
    pass [Only id] = id
    pass _         = 0
    
findSubCat pool id = do
    res <- liftIO $ fetch pool (Only id)
            "SELECT category_id FROM category WHERE  owner_id=?"   
    return $ pass res
    where pass [] = []
          pass xs = map fromOnly xs

--Tag-------------------------------------------------------------------------     
insertTag pool (Tag tag) = do
      liftIO $ execSqlT pool [tag]
        "INSERT INTO tag (tag) VALUES(?)"
      return ()


findTagByID pool id = do
     res <- liftIO $ fetch pool (Only id)
              "SELECT * FROM tag WHERE tag_id=?"  :: IO [(Integer, String)]   
     return $ pass res
         where pass [(id, tag)] = Just (Tag tag)
               pass _                  = Nothing
 
findTags pool id author = do
  res <- liftIO $ fetch pool [id, author]
             "SELECT tag FROM tag WHERE array_position ((SELECT tags FROM drafts WHERE id = ? AND author = ?), id) IS NOT NULL"
  return $ pass res
    where pass [] = []
          pass xs = map fromOnly xs
               
-- checkAvailabilityTags pool tags = do
     -- res <- liftIO $ fetch pool (Only (In tags))
              -- "SELECT id FROM tags WHERE id IN ?" 
     -- return $ pass res 
         -- where pass [] = []
               -- pass xs = map fromOnly xs
--Draft-------------------------------------------------------------------------------
insertDraft pool (DraftIn title category tags t_content mainPhoto otherPhotos) 
                  id c_date = do
  res <- liftIO $ fetch pool [ title
                             , c_date
                             , show id
                             , show category
                             , T.unpack t_content
                             , show mainPhoto]
           "INSERT INTO draft (title, draft_date, user_id, category_id, t_content, photo_id) VALUES(?,?,?,?,?,?) returning draft_id"
  return $ pass res
  where 
    pass [Only id] = id
    pass _         = 0

deleteDraft pool id author = do
  liftIO $ execSqlT pool [id, author] "DELETE FROM draft WHERE draft_id=? AND user_id =?" 
  return ()
    
updateDraft pool draft author = do
  case partQuery of
    "" -> return (Just draft)         
    _  -> do
         res <- liftIO $ fetch pool [id_draft draft, author] 
                $ fromString ("UPDATE draft SET" ++ partQuery ++
                              " WHERE draft_id=? AND user_id =? returning title, category_id, t_content, photo_id")
         return $ pass res                               
  where       
    partQuery = if sets == "" then "" else init sets
    sets = setTitle ++ setCategory ++ setContent ++ setPhoto
    setTitle =  case newTitle draft of 
                   Nothing -> "" 
                   Just title -> (" title = '" ++ title ++ "',")
    setCategory = case newCategory draft of 
                    Nothing -> ""
                    Just category -> (" category_id =" ++ (show category) ++ ",")
    setContent =  case newContent draft of 
                    Nothing -> ""
                    Just content -> (" t_content ='" ++ (T.unpack content) ++ "',")
    setPhoto = case newMainPhoto draft of 
                    Nothing -> ""
                    Just photo -> (" photo_id =" ++ (show photo) ++ ",")
    pass [(t, c, tc, p)] = 
         Just (draft { newTitle = newValue (newTitle draft) t
                     , newCategory = newValue (newCategory draft) c
                     , newContent = newValue (newContent  draft) tc
                     , newMainPhoto = newValue (newMainPhoto draft) p})                  
    pass _            = Nothing
    newValue a na = if a == Nothing then Nothing else Just na

insertTagDraft pool id tag = do
  res <- liftIO $ fetch pool [show tag, show id]
           "INSERT INTO tag_draft (tag_id, draft_id) VALUES(?,?) returning tag_id"
  return $ pass res
    where 
      pass [Only id] = id
      pass _         = 0

insertPhotoDraft pool id photo = do
  res <- liftIO $ fetch pool [show photo, show id]
           "INSERT INTO photo_draft (photo_id, draft_id) VALUES(?,?) returning photo_id"
  return $ pass res
    where 
      pass [Only id] = id
      pass _         = 0
-- updateDraft pool id author fild content_ = do
  -- res <- liftIO $ fetch pool [content, (show id), (show author)]  
                -- $ fromString ("UPDATE drafts SET " 
                              -- ++ fild ++ " =?"
                              -- ++ " WHERE id=? AND author =? returning id")

  -- return $ pass res
  -- where
    -- content = if ((fild == "tags") || (fild == "photos")) 
              -- then listToSql content_ 
              -- else           content_
    -- pass [Only id] = id
    -- pass _         = 0
    
-- findDraft pool id_d author = do
     -- res <- liftIO $ fetch pool [id_d, author]
              -- "SELECT id FROM drafts WHERE id=? AND author =?"
     -- return $ pass res
         -- where pass [Only id] = Just id
               -- pass _    = Nothing

findDraftByID pool id_d  = do
  res <- liftIO $ fetch pool [id_d]
             "SELECT title, draft_date :: varchar, category_id, ARRAY (SELECT tag_id FROM tag_draft WHERE draft.draft_id=tag_draft.draft_id) :: varchar, photo_id  :: varchar, ARRAY (SELECT photo_id FROM photo_draft WHERE draft.draft_id=photo_draft.draft_id):: varchar, t_content FROM draft WHERE draft.draft_id = ?"    
              :: IO [(String, String, Integer, String, String, String, T.Text)]
  return $ pass res
    where 
      pass [(title, c_date, id_cat, tags, photo, photos, t_content)]
            = Just (DraftGet title c_date id_cat
                             (read (listFromSql tags) :: [Integer])
                             ("http://localhost:3000/photo/" ++ photo)
                             (map fromPhotoId (read (listFromSql photos) :: [Integer]))
                             t_content)
      pass _ = Nothing

-- takeWholeDraft pool (id_d, id_a) = do
  -- res <- liftIO $ fetch pool [id_d, id_a]
              -- "SELECT id::varchar, title, c_date::varchar, author:: varchar, category::varchar, tags::varchar, photo::varchar, photos::varchar, t_content FROM drafts WHERE id=? AND author =?"
              -- :: IO [(String, String, String, String, String, String, String,
                      -- String, T.Text)]
  -- return $ pass res
    -- where 
      -- pass [(i, t, d, a, c, ts, p, ps, t_c)] = [i, t, d, a, c, ts, p, ps, T.unpack t_c]
      -- pass _ = []

publishPost pool draft author = do             
  liftIO $ execSqlT pool [draft, author] "DELETE FROM post WHERE draft_id=? AND user_id =?"  
  res <- liftIO $ fetch pool [draft, author]
           "INSERT INTO post (draft_id, title, draft_date, user_id, category_id, photo_id, t_content) SELECT * FROM draft WHERE draft_id=? AND user_id=? returning draft_id"
  return $ pass res
  where 
    pass [Only id] = id
    pass _         = 0

-- publishPost pool [] = return 0             
-- publishPost pool (i:t:d:a:xs) = do
  -- liftIO $ execSqlT pool [i, a] "DELETE FROM posts WHERE id=? AND author =?"  
  -- res <- liftIO $ fetch pool (i:t:d:a:xs)
           -- "INSERT INTO posts (id, title, c_date, author, category, tags, photo, photos, t_content) VALUES(?,?,?,?,?,?,?,?,?) returning id"
  -- return $ pass res
  -- where 
    -- pass [Only id] = id
    -- pass _         = 0
    

--Photo--------------------------------------------------------------------------------- 
insertPhoto pool (Photo im t)  = do
  res <- liftIO $ fetch pool [im, t]
        "INSERT INTO photo (image, image_type) VALUES (?,?) returning photo_id"
  return $ pass res
  where
        pass [Only id] = id
        pass _         = 0

findPhotoByID pool id = do
         res <- liftIO $ fetch pool (Only id)
                "SELECT image, image_type FROM photo WHERE photo_id=?"
                :: IO [(String, String)]
         return $ pass res
         where pass [(img, t)] = Just (img, t)
               pass _ = Nothing

-- updateMainPhoto pool author draft photo = do
  -- res <- liftIO $ fetch pool [(show draft), (show photo), (show author)]
         -- "UPDATE photo SET draft_id =?, main = TRUE WHERE photo_id =? AND user_id = ? returning photo_id"
  -- return $ pass res                  
  -- where
    -- pass [Only id] = id
    -- pass _         = 0
    
-- updateOtherPhotos pool author draft photos = do
  -- res <- liftIO $ fetch pool [draft, author]
             -- $ fromString ("UPDATE photo SET draft_id =?, main = FALSE WHERE  array_position (ARRAY" ++
             -- (show photos) ++
             -- ", photo_id) IS NOT NULL AND user_id = ? returning photo_id")
  -- return $ pass res
    -- where pass [] = []
          -- pass xs = map fromOnly xs


                
-- findPhoto pool id_p author = do
     -- res <- liftIO $ fetch pool [id_p, author] 
              -- "SELECT id FROM photos WHERE id=? AND author =?"
     -- return $ pass res
         -- where pass [Only id] = Just id
               -- pass _ = Nothing

-- checkAvailabilityPhotos pool photos author = do
     -- res <- liftIO $ fetch pool ((In photos), author)
              -- "SELECT id FROM photos WHERE id IN ? AND author = ?" 
     -- return $ pass res 
         -- where pass [] = []
               -- pass xs = map fromOnly xs
--Comment----------------------------------------------------------------------
insertComment pool (CommentIn post_id comment) 
                    author_id c_date = do
  res <- liftIO $ fetch pool [ c_date
                             , show post_id
                             , comment
                             , show author_id
                             ]
           "INSERT INTO comments (c_date, post_id, comment, user_id) VALUES(?,?,?,?) returning id"
  return $ pass res
  where 
    pass [Only id] = id
    pass _         = 0               
               
deleteComment pool id author = do
  liftIO $ execSqlT pool [id, author] "DELETE FROM comments WHERE id=? AND user_id =?" 
  return ()
-- Post------------------------------------------------------------------------
-- takeAllPosts' pool = do
  -- res <- fetchSimple pool "SELECT id, title, c_date :: varchar, author, category, tags :: varchar, photo, photos :: varchar, t_content FROM posts" 
     -- :: IO [(Integer, String, String, Integer, Integer, String, Integer, String, T.Text)]
  -- return $ map (\(i, t, d, a, c, ts, p, ps, t_c) ->
           -- Post i t d a c (read (listFromSql ts) :: [Integer])
                -- p (read (listFromSql ps) :: [Integer]) t_c) res

takeAllPosts pool = do
  res <- fetchSimple pool "SELECT p.id, p.title, p.c_date :: varchar, u.name, u.surname, c.name, p.tags :: varchar, p.photo, p.photos :: varchar, p.t_content FROM posts p, users u, categories c WHERE p.author = u.id AND p.category = c.id"
     :: IO [(Integer, String, String,
             String, String,
             String,
             String,
             Integer, String, T.Text)] 
  mapM joinPost res          
  where 
    -- joinPost :: (Integer, String, String, String, String, String, String, Integer, String, T.Text) -> IO Post
    joinPost (i, t, d, n, sn, c, ts, p, ps, t_c) = do
      tss <- mapM tagFromInteger (read (listFromSql ts) :: [Integer])
      return (Post i t d n sn c
                   tss
                   (fromPhotoId p)
                   (map fromPhotoId (read (listFromSql ps) :: [Integer]))
                   t_c)
    -- tagFromInteger :: Integer -> IO String           
    tagFromInteger id = do 
      nameTagMb <- findTagByID pool id
      case nameTagMb of
        Just (Tag nameTag) -> return nameTag
        Nothing            -> return ""
            
                
                
                
  -- return $ map (\(i, t, d, n, sn, c, ts, p, ps, t_c) ->
           -- Post i t d n sn c
                -- (read (listFromSql ts) :: [Integer])
                -- (fromPhotoId p)
                -- (map fromPhotoId (read (listFromSql ps) :: [Integer])) t_c) res
                
                
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
      where 
        retrieve conn = 
          (query conn sql args)
           `catches` [Handler (\ (ex :: SqlError)    -> handleSql ex),
                      Handler (\ (ex :: ResultError) -> handleSql ex)]
        handleSql ex = do
          putStrLn ("-------" ++ show ex)
          return []
          
                     -- do
              -- resEither <- try (query conn sql args)
              -- testException resEither
             
-- testException :: (Either SqlError [r]) ->  IO [r]
-- testException resEither = do
    -- case resEither of
        -- Right val -> return val
        -- Left ex   -> do
          -- putStrLn (show ex)
          -- return []
      
      

-- No arguments -- just pure sql
fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
       where retrieve conn = query_ conn sql

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

listFromSql :: String -> String
listFromSql list = init ('[': (tail list)) ++ "]"

      
fromPhotoId :: Integer -> String      
fromPhotoId id = "http://localhost:3000/photo/" ++ (show id)     
