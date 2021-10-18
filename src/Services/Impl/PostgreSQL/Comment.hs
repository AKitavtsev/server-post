{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL.Comment
  where

import Models.Comment
import Services.Impl.PostgreSQL.Internal

import Data.Pool
import Database.PostgreSQL.Simple


insertComment :: Pool Connection -> RawComment -> Integer -> String -> IO Integer
insertComment pool (RawComment p_id c) auth_id creation_date' = do
      let q =
            "INSERT INTO comment (comment_date, draft_id, comment, user_id) VALUES(?,?,?,?) returning comment_id"
      res <- fetch pool [creation_date', show p_id, c, show auth_id] q
      return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0

deleteComment :: Pool Connection -> Integer -> Integer -> IO ()
deleteComment pool id_ auth = do
      let q = "DELETE FROM comments WHERE id_=? AND user_id =?"
      _ <- execSqlT pool [id_, auth] q
      return ()
