{-# LANGUAGE OverloadedStrings #-}

module Services.Impl.PostgreSQL.User
  where

import Models.User
import Services.Impl.PostgreSQL.Internal
import Database.PostgreSQL.Simple

import Data.Pool

insertUser :: Pool Connection -> RawUser -> String -> IO Integer
insertUser pool (RawUser name' surname' _ login' password') c_date' = do
      let q =
            "INSERT INTO user_ (user_name, surname, login, password, user_date, admin) VALUES(?,?,?,md5( ?) ,?,?) returning user_id"
      res <- fetch pool [name', surname', login', password', c_date', "FALSE"] q
      return $ pass res
      where
        pass [Only id_] = id_
        pass _ = 0
        
findUserByLogin :: Pool Connection -> String -> String -> IO (Maybe ( Integer, Bool))
findUserByLogin pool login' password' = do
      let q =
            "SELECT user_id, admin  FROM user_ WHERE login =? AND password = md5( ?)"
      res <- fetch pool [login', password'] q :: IO [(Integer, Bool)]
      return $ pass res
      where
        pass [(id_, adm)] = Just (id_, adm)
        pass _ = Nothing
        
findUserByID :: String -> Pool Connection -> Integer -> IO (Maybe ForShowUser)
findUserByID hostPort pool id_ = do
      let q =
            "SELECT user_name, surname, login, user_date::varchar, admin  FROM user_ WHERE user_id=?"
      res <-
        fetch pool (Only id_) q :: IO [(String, String, String, String, Bool)]
      return $ pass res
      where
        pass [(n, sn, l, dat, adm)] =
          Just
            (ForShowUser n sn (hostPort ++ "/image/" ++ show id_) l dat adm)
        pass _ = Nothing
        
insertImage :: Pool Connection -> RawUser -> Integer -> IO Integer
insertImage pool (RawUser _ _ avatar' _ _) id_ = do
      case avatar' of
        Just (Avatar im t) -> do
          let q =
                "INSERT INTO image (user_id, image, image_type) VALUES (?,?,?) returning user_id"
          res <- fetch pool [show id_, im, t] q
          return $ pass res
          where pass [Only i] = i
                pass _ = 0
        Nothing -> return (-1)
findImageByID :: Pool Connection -> Integer -> IO (Maybe (String, String))
findImageByID pool id_ = do
      let q = "SELECT image, image_type FROM image WHERE user_id=?"
      res <- fetch pool (Only id_) q :: IO [(String, String)]
      return $ pass res
      where
        pass [(img, t)] = Just (img, t)
        pass _ = Nothing
