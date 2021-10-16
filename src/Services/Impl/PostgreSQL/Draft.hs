{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL.Draft
  where

import Models.Draft
import Services.Impl.PostgreSQL.Internal

import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Pool
import Database.PostgreSQL.Simple

import qualified Data.Text as T


insertDraft :: Pool Connection -> RawDraft -> Integer -> String -> IO Integer
insertDraft pool (RawDraft t c _ t_c m_p _) id_ c_date' = do
      let q =
            "INSERT INTO draft (title, draft_date, user_id, category_id, t_content, photo_id) VALUES(?,?,?,?,?,?) returning draft_id"
      res <- fetch pool [t, c_date', show id_, show c, T.unpack t_c, show m_p] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0

findDraftByID :: String -> Pool Connection -> Integer -> IO (Maybe ForShowDraft)
findDraftByID hostPort pool id_d = do
      let q =
            "SELECT title, draft_date :: varchar, category_id, ARRAY (SELECT tag_id FROM tag_draft WHERE draft.draft_id=tag_draft.draft_id) :: varchar, photo_id  :: varchar, ARRAY (SELECT photo_id FROM photo_draft WHERE draft.draft_id=photo_draft.draft_id):: varchar, t_content FROM draft WHERE draft.draft_id = ?"
      res <-
        fetch pool [id_d] q :: IO [( String
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
            (ForShowDraft
               t
               c_date'
               id_cat
               (toListInteger ts)
               (hostPort ++ "/photo/" ++ ph)
               (map (fromPhotoId hostPort) (toListInteger phs))
               t_c)
        pass _ = Nothing
        
deleteDraft :: Pool Connection -> Integer -> Integer -> IO ()
deleteDraft pool id_ auth = do
      let q = "DELETE FROM draft WHERE draft_id=? AND user_id =?"
      _ <- execSqlT pool [id_, auth] q
      return ()
      
updateDraft :: Pool Connection -> ForUpdateDraft -> Integer -> IO (Maybe ForUpdateDraft)
updateDraft pool draft auth = do
      case bodyUpdate draft of
        ("", _) -> return (Just draft)
        (textQuery, paramQuery) -> do
          res <-
            fetch pool (paramQuery <> [show (id_draft draft), show auth]) $
            fromString
              ("UPDATE draft SET" <> textQuery <>
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
            
insertTagDraft :: Pool Connection -> Integer -> Integer -> IO Integer            
insertTagDraft pool id_ t = do
      let q =
            "INSERT INTO tag_draft (tag_id, draft_id) VALUES(?,?) returning tag_id"
      res <- fetch pool [show t, show id_] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
        
insertPhotoDraft :: Pool Connection -> Integer -> Integer -> IO Integer       
insertPhotoDraft pool id_ ph = do
      let q =
            "INSERT INTO photo_draft (photo_id, draft_id) VALUES(?,?) returning photo_id"
      res <- fetch pool [show ph, show id_] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
        

insertPhoto :: Pool Connection -> Photo -> IO Integer
insertPhoto pool (Photo im t) = do
      let q =
            "INSERT INTO photo (image, image_type) VALUES (?,?) returning photo_id"
      res <- fetch pool [im, t] q
      return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0
        
findPhotoByID :: Pool Connection -> Integer -> IO (Maybe (String, String))        
findPhotoByID pool id_ = do
      let q = "SELECT image, image_type FROM photo WHERE photo_id=?"
      res <- fetch pool (Only id_) q :: IO [(String, String)]
      return $ pass res
      where
        pass [(img, t)] = Just (img, t)
        pass _ = Nothing
        
publishPost :: Pool Connection -> Integer -> Integer -> IO Integer
publishPost pool draft auth = do
      let queryFindDraft =
            "SELECT draft_id, title, draft_date:: varchar, user_id, category_id, photo_id, t_content FROM draft WHERE draft_id=? AND user_id=?"
      fetch pool [draft, auth] (fromString queryFindDraft) >>=
        updateOrInsertPost
      where
        updateOrInsertPost ::
             [(Integer, String, String, Integer, Integer, Integer, T.Text)]
          -> IO Integer
        updateOrInsertPost [(i, t, d, u, c, p, t_c)] = do
          let queryUpdatePost =
                "UPDATE post SET title=?, category_id=?, photo_id=?, t_content=? WHERE draft_id=? AND user_id =? returning draft_id"
              queryInsertPost =
                "INSERT INTO post (draft_id, title, draft_date, user_id, category_id, photo_id, t_content) VALUES(?,?,?,?,?,?,?) returning draft_id"
          res <-
            fetch pool [t, show c, show p, T.unpack t_c, show i, show u] $
            fromString queryUpdatePost
          case res of
            [Only idPost] -> return idPost
            _ -> do
              res' <-
                fetch pool [show i, t, d, show u, show c, show p, T.unpack t_c] $
                fromString queryInsertPost :: IO [Only Integer]
              case res' of
                [Only idPost] -> return idPost
                _ -> return 0
        updateOrInsertPost _ = return 0
