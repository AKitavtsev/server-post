{-# LANGUAGE OverloadedStrings #-}

module Servises.Impl.PostgreSQL.Pagination 
  where

import FromRequest

import Control.Monad.Trans (liftIO)
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.Types (Query (..))
import Data.Pool (Pool (..), withResource)
import GHC.Int (Int64 (..))

newPagination :: Pool Connection -> String -> Integer -> String -> IO ()
newPagination pool model user_id halfQuery = do
  liftIO $ execSql pool [show user_id, model]
           "DELETE FROM pagination WHERE user_id=? AND model = ?"
  liftIO $ execSql pool [show user_id, model, halfQuery]
           "INSERT INTO pagination (user_id, model, part_query) VALUES(?,?,?)"
  return ()
  
continuePagination :: Pool Connection -> String -> Integer -> IO String
continuePagination pool model user_id  = do
  res <- liftIO $ fetch pool [show user_id, model]
              "SELECT part_query FROM pagination WHERE user_id =? AND model =?"   
  return $ pass res
    where pass [Only pq] = pq

-- findTags pool id author = do
  -- res <- liftIO $ fetch pool [id, author]
             -- "SELECT tag FROM tag WHERE array_position ((SELECT tags FROM drafts WHERE id = ? AND author = ?), id) IS NOT NULL"
  -- return $ pass res
    -- where pass [] = []
          -- pass xs = map fromOnly xs
 
execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSql pool args sql = withResource pool ins
       where ins conn = execute conn sql args
       
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
      where retrieve conn = query conn sql args

-- fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
-- fetch pool args sql = withResource pool retrieve
      -- where 
        -- retrieve conn = 
          -- (query conn sql args)
           -- `catches` [Handler (\ (ex :: SqlError)    -> handleSql ex),
                      -- Handler (\ (ex :: ResultError) -> handleSql ex)]
        -- handleSql ex = do
          -- putStrLn ("-------" ++ show ex)
          -- return []