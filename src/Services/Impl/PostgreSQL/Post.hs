{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL.Post
  where

import FromRequest
import Models.Author
import Models.Post
import Services.Impl.PostgreSQL.Internal

import Data.String (fromString)
import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai (Request(..))


findAllPosts :: String -> Pool Connection -> Request -> Integer -> IO [Post]
findAllPosts hostPort pool req limit = do
      let endQuery = queryWhereOrder req limit
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
               (AuthorsDetails
                  user_name
                  surname'
                  descr
                  (hostPort ++ "/image/" ++ user_id))
               (Categories c subcatName)
               (toListString ts)
               (hostPort ++ "/photo/" ++ ph)
               (map (fromPhotoId hostPort) (toListInteger phs))
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
          res <- fetch pool' (Only i) q
          return (map fromOnly res)
        findNameSC pool' i = do
          let q = "SELECT category_name FROM category WHERE  category_id=?"
          res <- fetch pool' (Only i) q
          return $ pass res
          where
            pass [Only n] = n
            pass _ = ""
        getNameSC = findNameSC pool

findComments :: String -> Pool Connection -> Request -> Integer -> Integer -> IO [Comment]        
findComments hostPort pool req limit id_post = do
      let offset =
            case toParam req "page" of
              Nothing -> 0
              Just page -> limit * (numberPage page - 1)
          q =
            "SELECT comment_date :: varchar," ++
            " user_id ::varchar, user_name, surname, comment " ++
            "FROM comment INNER JOIN user_ USING (user_id) " ++
            "WHERE draft_id = ? LIMIT ? OFFSET ?" 
      res <- fetch pool [show id_post, show limit, show offset] $ fromString  q
      return (map toComment res)
      where
        toComment (comment_data, user_id, user_name, surn, com) =
          Comment
            comment_data
            (User user_name surn (hostPort ++ "/image/" ++ user_id))
            com
