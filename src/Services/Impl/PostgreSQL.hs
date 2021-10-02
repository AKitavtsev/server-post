{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL
  ( newHandle
  ) where

import qualified Config as C
import qualified Services.Db as SD

import FromRequest
import Models.Author
import Models.Category
import Models.Comment
import Models.Draft
import Models.Post
import Models.Tag
import Models.User
import Services.Impl.PostgreSQL.Internal
import Services.Impl.PostgreSQL.Migrations

import Control.Monad.Trans (liftIO)
import Data.Char (toLower)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import GHC.Int (Int64(..))

import qualified Data.Text as T

newHandle :: C.Config -> IO SD.Handle
newHandle config = do
  return $
    SD.Handle
      { SD.limit = C.limit config
      , SD.close = close
      , SD.newConn = newConn
      , SD.runMigrations = runMigrations
      , SD.deleteByID = deleteByID
      , SD.updateByID = updateByID
      , SD.insertUser = insertUser
      , SD.findUserByLogin = findUserByLogin
      , SD.findUserByID = findUserByID
      , SD.insertImage = insertImage
      , SD.findImageByID = findImageByID
      , SD.insertAuthor = insertAuthor
      , SD.findAuthorByID = findAuthorByID
      , SD.insertCategory = insertCategory
      , SD.findCategoryByID = findCategoryByID
      , SD.updateOwnerCategory = updateOwnerCategory
      , SD.insertTag = insertTag
      , SD.insertTagDraft = insertTagDraft
      , SD.insertPhotoDraft = insertPhotoDraft
      , SD.findTags = findTags
      , SD.insertDraft = insertDraft
      , SD.deleteDraft = deleteDraft
      , SD.updateDraft = updateDraft
      , SD.insertPhoto = insertPhoto
      , SD.findTagByID = findTagByID
      , SD.findPhotoByID = findPhotoByID
      , SD.findDraftByID = findDraftByID
      , SD.publishPost = publishPost
      , SD.insertComment = insertComment
      , SD.deleteComment = deleteComment
      , SD.findAllPosts = findAllPosts
      , SD.findComments = findComments
      }
  where
    newConn conf =
      connect
        defaultConnectInfo
          { connectUser = C.user conf
          , connectPassword = C.password conf
          , connectDatabase = C.name conf
          }
    deleteByID pool model id_ = do
      _ <-
        case model of
          "user" ->
            liftIO $ execSqlT pool [id_] "DELETE FROM user_ WHERE user_id=?"
          "author" ->
            liftIO $ execSqlT pool [id_] "DELETE FROM author WHERE user_id=?"
          "category" ->
            liftIO $
            execSqlT pool [id_] "DELETE FROM category WHERE category_id=?"
          "tag" -> liftIO $ execSqlT pool [id_] "DELETE FROM tag WHERE tag_id=?"
          "comment" ->
            liftIO $ execSqlT pool [id_] "DELETE FROM comment WHERE id_=?"
          "tag_draft" ->
            liftIO $
            execSqlT pool [id_] "DELETE FROM  tag_draft WHERE draft_id=?"
          "photo_draft" ->
            liftIO $
            execSqlT pool [id_] "DELETE FROM  photo_draft WHERE draft_id=?"
          _ -> return (0 :: Int64)
      return ()
    updateByID pool model id_ value = do
      _ <-
        case model of
          "author" -> do
            liftIO $
              execSqlT
                pool
                [value, show id_]
                "UPDATE author SET description =? WHERE user_id=?"
          "tag" -> do
            liftIO $
              execSqlT
                pool
                [value, show id_]
                "UPDATE tag SET tag=? WHERE tag_id=?"
          "category" -> do
            liftIO $
              execSqlT
                pool
                [value, show id_]
                "UPDATE category SET category_name=? WHERE category_id=?"
          _ -> return (0 :: Int64)
      return ()
    --User---------------------------------------------------------------------
    insertUser pool (UserIn name' surname' _ login' password') c_date' = do
      let q =
            "INSERT INTO user_ (user_name, surname, login, password, user_date, admin) VALUES(?,?,?,md5( ?) ,?,?) returning user_id"
      res <-
        liftIO $
        fetch pool [name', surname', login', password', c_date', "FALSE"] q
      return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0
    findUserByLogin pool login' password' = do
      let q =
            "SELECT user_id, admin  FROM user_ WHERE login =? AND password = md5( ?)"
      res <- liftIO $ fetch pool [login', password'] q :: IO [(Integer, Bool)]
      return $ pass res
      where
        pass [(id_, adm)] = Just (id_, adm)
        pass _ = Nothing
    findUserByID pool id_ = do
      let q =
            "SELECT user_name, surname, login, user_date::varchar, admin  FROM user_ WHERE user_id=?"
      res <-
        liftIO $ fetch pool (Only id_) q :: IO [( String
                                              , String
                                              , String
                                              , String
                                              , Bool)]
      return $ pass res
      where
        pass [(n, sn, l, dat, adm)] =
          Just
            (UserOut n sn ("http://localhost:3000/image/" ++ show id_) l dat adm)
        pass _ = Nothing
    insertImage pool (UserIn _ _ avatar' _ _) id_ = do
      case avatar' of
        Just (Avatar im t) -> do
          let q =
                "INSERT INTO image (user_id, image, image_type) VALUES (?,?,?) returning user_id"
          res <- liftIO $ fetch pool [show id_, im, t] q
          return $ pass res
          where pass [Only i] = i
                pass _ = 0
        Nothing -> return (-1)
    findImageByID pool id_ = do
      let q = "SELECT image, image_type FROM image WHERE user_id=?"
      res <- liftIO $ fetch pool (Only id_) q :: IO [(String, String)]
      return $ pass res
      where
        pass [(img, t)] = Just (img, t)
        pass _ = Nothing
    --Author------------------------------------------
    insertAuthor pool (Author id_ descr) = do
      let q =
            "INSERT INTO author  (user_id, description) VALUES(?,?) returning user_id"
      res <- liftIO $ fetch pool [show id_, T.unpack descr] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
    findAuthorByID pool id_ = do
      let q =
            "SELECT user_name, surname, description FROM user_ INNER JOIN author USING(user_id) WHERE user_.user_id = ?;"
      res <- liftIO $ fetch pool (Only id_) q
      return $ pass res
      where
        pass [(name', surname', descr)] =
          Just
            (AuthorOut
               name'
               surname'
               descr
               ("http://localhost:3000/image/" ++ show id_))
        pass _ = Nothing
    --Category------------------------------
    insertCategory pool (Category name' owner_id) = do
      case owner_id of
        Just owner -> do
          let q =
                "INSERT INTO category (category_name, owner_id) VALUES(?,?) returning category_id"
          res <- liftIO $ fetch pool [name', show owner] q
          return $ pass res
        Nothing -> do
          let q =
                "INSERT INTO category (category_name) VALUES(?) returning category_id"
          res <- liftIO $ fetch pool [name'] q
          return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0
    findCategoryByID pool id_ = do
      let q = "SELECT * FROM category WHERE category_id=?"
      res <-
        liftIO $ fetch pool (Only id_) q :: IO [(Integer, String, Maybe Integer)]
      return $ pass res
      where
        pass [(_, name', idOw)] = Just (Category name' idOw)
        pass _ = Nothing
    updateOwnerCategory pool id_ owner = do
      case map toLower owner of
        "null" -> do
          let q =
                "UPDATE category SET owner_id=null WHERE category_id=? returning category_id"
          res <- liftIO $ fetch pool [show id_] q
          return $ pass res
        _ -> do
          let q =
                "UPDATE category SET owner_id=? WHERE category_id=? returning category_id"
          res <- liftIO $ fetch pool [owner, show id_] q
          return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
    --Tag-------------------------------------------------------------------------
    insertTag pool (Tag t) = do
      _ <- liftIO $ execSqlT pool [t] "INSERT INTO tag (tag) VALUES(?)"
      return ()
    findTagByID pool id_ = do
      let q = "SELECT * FROM tag WHERE tag_id=?"
      res <- liftIO $ fetch pool (Only id_) q :: IO [(Integer, String)]
      return $ pass res
      where
        pass [(_, t)] = Just (Tag t)
        pass _ = Nothing
    findTags pool id_ auth = do
      let q =
            "SELECT tag FROM tag WHERE array_position ((SELECT tags FROM drafts WHERE id_ = ? AND author = ?), id_) IS NOT NULL"
      res <- liftIO $ fetch pool [id_, auth] q
      return $ pass res
      where
        pass [] = []
        pass xs = map fromOnly xs
    --Draft------------------------------------------------------------------------
    insertDraft pool (DraftIn t c _ t_c m_p _) id_ c_date' = do
      let q =
            "INSERT INTO draft (title, draft_date, user_id, category_id, t_content, photo_id) VALUES(?,?,?,?,?,?) returning draft_id"
      res <-
        liftIO $
        fetch pool [t, c_date', show id_, show c, T.unpack t_c, show m_p] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
    deleteDraft pool id_ auth = do
      let q = "DELETE FROM draft WHERE draft_id=? AND user_id =?"
      _ <- liftIO $ execSqlT pool [id_, auth] q
      return ()
    updateDraft pool draft auth = do
      case bodyUpdate draft of
        ("", _) -> return (Just draft)
        (textQuery, paramQuery)-> do
          res <-
            liftIO $
            fetch pool (paramQuery <> [show (id_draft draft), show auth]) $
            fromString
              ("UPDATE draft SET" <>
               textQuery <>
               " WHERE draft_id=? AND user_id =? returning title, category_id, t_content, photo_id")
          return $ pass res
      where
        pass [(t, c, tc, p)] =
          Just
            (draft
               { new_title = newValue (new_title draft) t
               , new_category = newValue (new_category draft) c
               , new_content = newValue (new_content draft) tc
               , new_main_photo = newValue (new_main_photo draft) p
               })
        pass _ = Nothing
        newValue a na =
          if isNothing a
            then Nothing
            else Just na
    insertTagDraft pool id_ t = do
      let q =
            "INSERT INTO tag_draft (tag_id, draft_id) VALUES(?,?) returning tag_id"
      res <- liftIO $ fetch pool [show t, show id_] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
    insertPhotoDraft pool id_ ph = do
      let q =
            "INSERT INTO photo_draft (photo_id, draft_id) VALUES(?,?) returning photo_id"
      res <- liftIO $ fetch pool [show ph, show id_] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
    findDraftByID pool id_d = do
      let q =
            "SELECT title, draft_date :: varchar, category_id, ARRAY (SELECT tag_id FROM tag_draft WHERE draft.draft_id=tag_draft.draft_id) :: varchar, photo_id  :: varchar, ARRAY (SELECT photo_id FROM photo_draft WHERE draft.draft_id=photo_draft.draft_id):: varchar, t_content FROM draft WHERE draft.draft_id = ?"
      res <-
        liftIO $ fetch pool [id_d] q :: IO [( String
                                            , String
                                            , Integer
                                            , String
                                            , String
                                            , String
                                            , T.Text)]
      return $ pass res
      where
        pass [(t, c_date', id_cat, ts, ph, phs, t_c)] =
          Just
            (DraftGet
               t
               c_date'
               id_cat
               (toListInteger ts)
               ("http://localhost:3000/photo/" ++ ph)
               (map fromPhotoId (toListInteger phs))
               t_c)
        pass _ = Nothing        
    publishPost pool draft auth = do      
      let queryFindDraft = "SELECT draft_id, title, draft_date:: varchar, user_id, category_id, photo_id, t_content FROM draft WHERE draft_id=? AND user_id=?"
      fetch pool [draft, auth] (fromString queryFindDraft) >>= updateOrInsertPost
      where
        updateOrInsertPost ::
          [(Integer, String, String, Integer, Integer, Integer, T.Text)] -> IO Integer
        updateOrInsertPost [(i, t, d, u, c, p, t_c)] = do
          let queryUpdatePost = "UPDATE post SET title=?, category_id=?, photo_id=?, t_content=? WHERE draft_id=? AND user_id =? returning draft_id"
              queryInsertPost = "INSERT INTO post (draft_id, title, draft_date, user_id, category_id, photo_id, t_content) VALUES(?,?,?,?,?,?,?) returning draft_id"
          res <- fetch pool 
                       [t, show c, show p, T.unpack t_c, show i, show u]
                       $ fromString queryUpdatePost 
          case res of
             [Only idPost] -> return idPost
             _ -> do
               res' <- fetch pool 
                             [show i, t, d, show u, show c, show p, T.unpack t_c]
                             $ fromString queryInsertPost :: IO [Only Integer]
               case res' of
                [Only idPost] -> return idPost
                _ -> return 0
        updateOrInsertPost _ = return 0
    --Photo----------------------------------------------------------------------
    insertPhoto pool (Photo im t) = do
      let q =
            "INSERT INTO photo (image, image_type) VALUES (?,?) returning photo_id"
      res <- liftIO $ fetch pool [im, t] q
      return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0
    findPhotoByID pool id_ = do
      let q = "SELECT image, image_type FROM photo WHERE photo_id=?"
      res <- liftIO $ fetch pool (Only id_) q :: IO [(String, String)]
      return $ pass res
      where
        pass [(img, t)] = Just (img, t)
        pass _ = Nothing
    --Comment----------------------------------------------------------------------
    insertComment pool (CommentIn p_id c) auth_id c_date' = do
      let q =
            "INSERT INTO comment (comment_date, draft_id, comment, user_id) VALUES(?,?,?,?) returning comment_id"
      res <- liftIO $ fetch pool [c_date', show p_id, c, show auth_id] q
      return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0
    deleteComment pool id_ auth = do
      let q = "DELETE FROM comments WHERE id_=? AND user_id =?"
      _ <- liftIO $ execSqlT pool [id_, auth] q
      return ()
    -- Post------------------------------------------------------------------------
    findAllPosts pool req limit = do
      let endQuery= queryWhereOrder req limit
      let q =
            "WITH " ++
            "gettags (t_id, t_name, d_id)" ++
            " AS (SELECT tag_id, tag, draft_id" ++
            " FROM tag_draft" ++
            " INNER JOIN tag USING (tag_id)" ++
            " INNER JoIN post USING (draft_id))," ++
            "getphotos (p_id, d_id)" ++
            " AS (SELECT photo_draft.photo_id, draft_id" ++
            " FROM photo_draft" ++
            " INNER JOIN post USING (draft_id)) " ++
            "SELECT draft_id, title," ++
            " draft_date :: varchar," ++
            " category_name, category_id," ++
            " user_id::varchar , user_name, surname, description," ++
            " ARRAY (SELECT t_name" ++
            " FROM gettags" ++
            " WHERE d_id = draft_id):: varchar," ++
            " photo_id  :: varchar," ++
            " ARRAY (SELECT p_id" ++
            " FROM getphotos" ++
            " WHERE d_id = draft_id):: varchar," ++
            " t_content " ++
            "FROM post" ++
            " INNER JOIN user_ USING (user_id)" ++
            " INNER JOIN author USING (user_id)" ++
            " INNER JOIN category USING (category_id)" ++ fst endQuery
      -- putStrLn ( "Query: " ++ q ++ "\n" ++ "paramQuery: " ++ (show (snd endQuery)))
      res <- fetch pool (snd endQuery) (fromString q)
      mapM toPost res
      where
        toPost (i, t, c_date', c, category_id, user_id, user_name, surname', descr, ts, ph, phs, t_c) = do
          subcat <- allCategories [category_id] []
          subcatName <- mapM getNameSC subcat
          return
            (Post
               i
               t
               c_date'
               (AuthorOut
                  user_name
                  surname'
                  descr
                  ("http://localhost:3000/image/" ++ user_id))
               (Categories c subcatName)
               (toListString ts)
               ("http://localhost:3000/photo/" ++ ph)
               (map fromPhotoId (toListInteger phs))
               t_c)
        allCategories :: [Integer] -> [Integer] -> IO [Integer]
        allCategories [] tg = return tg
        allCategories xs tg = do
          subxs <- mapM getSubCat xs
          allCategories (concat subxs) (tg ++ concat subxs)
          where
            getSubCat = findSubCat pool
        findSubCat pool' i = do
          let q = "SELECT category_id FROM category WHERE  owner_id=?"
          res <- liftIO $ fetch pool' (Only i) q
          return (map fromOnly res)
        findNameSC pool' i = do
          let q = "SELECT category_name FROM category WHERE  category_id=?"
          res <- liftIO $ fetch pool' (Only i) q
          return $ pass res
          where
            pass [Only n] = n
            pass _ = ""
        getNameSC = findNameSC pool
    findComments pool req limit id_post = do
      let offset =
            case toParam req "page" of
              Nothing -> 0
              Just page -> limit * (read' page - 1)
          q =
            "SELECT comment_date :: varchar," ++
            " user_id ::varchar, user_name, surname, comment " ++
            "FROM comment INNER JOIN user_ USING (user_id) " ++
            "WHERE draft_id = ? LIMIT ? OFFSET ?" 
      -- putStrLn ( "Query: " ++ q ++ "\n" ++ "paramQuery: " ++ (show ([show id_post, show limit, show offset])))
      res <- fetch pool [show id_post, show limit, show offset] $ fromString  q
      return (map toComment res)
      where
        toComment (comment_data, user_id, user_name, surn, com) =
          Comment
            comment_data
            (User user_name surn ("http://localhost:3000/image/" ++ user_id))
            com
