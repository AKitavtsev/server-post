{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL.Category
  where

import Models.Category
import Services.Impl.PostgreSQL.Internal

import Data.Char (toLower)
import Data.Pool
import Database.PostgreSQL.Simple


insertCategory :: Pool Connection -> Category -> IO Integer
insertCategory pool (Category name' parent_id) = do
      case parent_id of
        Just parent -> do
          let q =
                "INSERT INTO category (category_name, owner_id) VALUES(?,?) returning category_id"
          res <- fetch pool [name', show parent] q
          return $ pass res
        Nothing -> do
          let q =
                "INSERT INTO category (category_name) VALUES(?) returning category_id"
          res <- fetch pool [name'] q
          return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0

findCategoryByID :: Pool Connection -> Integer -> IO (Maybe Category)
findCategoryByID pool id_ = do
      let q = "SELECT * FROM category WHERE category_id=?"
      res <- fetch pool (Only id_) q :: IO [(Integer, String, Maybe Integer)]
      return $ pass res
      where
        pass [(_, name', idOw)] = Just (Category name' idOw)
        pass _ = Nothing

updateParentCategory :: Pool Connection -> Integer -> String -> IO Integer
updateParentCategory pool id_ parent = do
      case map toLower parent of
        "null" -> do
          let q =
                "UPDATE category SET owner_id=null WHERE category_id=? returning category_id"
          res <- fetch pool [show id_] q
          return $ pass res
        _ -> do
          let q =
                "UPDATE category SET owner_id=? WHERE category_id=? returning category_id"
          res <- fetch pool [parent, show id_] q
          return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
