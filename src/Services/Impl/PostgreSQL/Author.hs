{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL.Author
  where

import Models.Author
import Services.Impl.PostgreSQL.Internal

import Data.Pool
import Database.PostgreSQL.Simple

import qualified Data.Text as T


insertAuthor :: Pool Connection -> RawAuthor -> IO Integer
insertAuthor pool (RawAuthor id_ descr) = do
      let q =
            "INSERT INTO author  (user_id, description) VALUES(?,?) returning user_id"
      res <- fetch pool [show id_, T.unpack descr] q
      return $ pass res
      where
        pass [Only i] = i
        pass _ = 0
        
findAuthorByID :: Pool Connection -> Integer -> IO (Maybe AuthorsDetails)
findAuthorByID pool id_ = do
      let q =
            "SELECT user_name, surname, description FROM user_ INNER JOIN author USING(user_id) WHERE user_.user_id = ?;"
      res <- fetch pool (Only id_) q
      return $ pass res
      where
        pass [(name', surname', descr)] =
          Just
            (AuthorsDetails
               name'
               surname'
               descr
               ("http://localhost:3000/image/" ++ show id_))
        pass _ = Nothing
