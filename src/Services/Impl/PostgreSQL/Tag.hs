{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL.Tag
  where

import Models.Tag
import Services.Impl.PostgreSQL.Internal

import Data.Pool
import Database.PostgreSQL.Simple


insertTag :: Pool Connection -> Tag -> IO ()
insertTag pool (Tag t) = do
      _ <- execSqlT pool [t] "INSERT INTO tag (tag) VALUES(?)"
      return ()

findTagByID :: Pool Connection -> Integer -> IO (Maybe Tag)
findTagByID pool id_ = do
      let q = "SELECT * FROM tag WHERE tag_id=?"
      res <- fetch pool (Only id_) q :: IO [(Integer, String)]
      return $ pass res
      where
        pass [(_, t)] = Just (Tag t)
        pass _ = Nothing

findTags :: Pool Connection -> Integer -> Integer -> IO [String]
findTags pool id_ auth = do
      let q =
            "SELECT tag FROM tag WHERE array_position ((SELECT tags FROM drafts WHERE id_ = ? AND author = ?), id_) IS NOT NULL"
      res <- fetch pool [id_, auth] q
      return $ pass res
      where
        pass [] = []
        pass xs = map fromOnly xs

