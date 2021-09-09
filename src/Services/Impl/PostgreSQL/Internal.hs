{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Services.Impl.PostgreSQL.Internal where

-- import Services.Impl.PostgreSQL.Pagination
import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad.Trans (liftIO)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool(..), withResource)
import Database.PostgreSQL.Simple
import GHC.Int (Int64(..))
import Network.Wai (Request(..))

import qualified Data.Text as T (unpack)

import FromRequest
import Models.Draft (DraftUp(..))

-- Forms part of the querry for UPDATE draft
partQuery :: DraftUp -> String
partQuery draft =
  if sets == ""
    then ""
    else init sets
  where
    sets = setTitle ++ setCategory ++ setContent ++ setPhoto
    setTitle =
      case newTitle draft of
        Nothing -> ""
        Just title -> " title = '" ++ title ++ "',"
    setCategory =
      case newCategory draft of
        Nothing -> ""
        Just category -> " category_id =" ++ show category ++ ","
    setContent =
      case newContent draft of
        Nothing -> ""
        Just content -> " t_content ='" ++ T.unpack content ++ "',"
    setPhoto =
      case newMainPhoto draft of
        Nothing -> ""
        Just photo -> " photo_id =" ++ show photo ++ ","

-- Forms part of the querry for SELECT post
queryWhereOrder :: Pool Connection -> Request -> Integer -> Integer -> IO String
queryWhereOrder pool req limit id_user =
  case toParam req "page" of
    Nothing -> do
      newPagination pool "post" id_user (queryWhere req ++ queryOrder req)
      return (queryWhere req ++ queryOrder req ++ " LIMIT " ++ show limit)
    Just page -> do
      q <- continuePagination pool "post" id_user
      return
        (q ++
         "LIMIT " ++ show limit ++ "OFFSET " ++ show (limit * (read' page - 1)))

-- Forms part of the querry for SELECT post WHERE
queryWhere :: Request -> String
queryWhere req =
  case qw of
    "" -> ""
    _ -> " WHERE" ++ reverse (drop 4 $ reverse qw)
  where
    qw =
      queryWhereTag req ++
      queryWhereTitle req ++
      queryWhereText req ++
      queryWhereDate req ++
      queryWhereAuthor req ++ queryWhereCategory req ++ queryWhereFind req

-- part of the querry for SELECT post WHERE tag
queryWhereTag :: Request -> String
queryWhereTag req = fromMaybe "" (tag <|> tagsIn <|> tagsAll)
  where
    array = " ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id)"
    tag =
      case toParam req "tag" of
        Nothing -> Nothing
        Just t ->
          Just
            (" array_position " ++
             "(" ++ array ++ "," ++ t ++ ") IS NOT NULL AND")
    tagsIn =
      case toParam req "tags_in" of
        Nothing -> Nothing
        Just t -> Just (" " ++ array ++ " && ARRAY " ++ t ++ " AND")
    tagsAll =
      case toParam req "tags_all" of
        Nothing -> Nothing
        Just t -> Just (" " ++ array ++ " @> ARRAY " ++ t ++ " AND")

-- part of the querry for SELECT post WHERE title
queryWhereTitle :: Request -> String
queryWhereTitle req =
  case toParam req "title" of
    Nothing -> ""
    Just t -> " title LIKE '%" ++ t ++ "%' AND"

-- for text content
queryWhereText :: Request -> String
queryWhereText req =
  case toParam req "text" of
    Nothing -> ""
    Just t -> " t_content LIKE '%" ++ t ++ "%' AND"

-- for date (created)
queryWhereDate :: Request -> String
queryWhereDate req = fromMaybe "" (dateAT <|> dateLT <|> dateGT)
  where
    beg = " draft_date :: date "
    dateAT =
      case toParam req "created_at" of
        Nothing -> Nothing
        Just t -> Just (beg ++ "='" ++ t ++ "' AND")
    dateLT =
      case toParam req "created_lt" of
        Nothing -> Nothing
        Just t -> Just (beg ++ "<'" ++ t ++ "' AND")
    dateGT =
      case toParam req "created_gt" of
        Nothing -> Nothing
        Just t -> Just (beg ++ ">'" ++ t ++ "' AND")

-- for author (name)
queryWhereAuthor :: Request -> String
queryWhereAuthor req =
  case toParam req "name" of
    Nothing -> ""
    Just t -> " user_name = '" ++ t ++ "' AND"

-- for categury_id
queryWhereCategory :: Request -> String
queryWhereCategory req =
  case toParam req "category" of
    Nothing -> ""
    Just t -> " category_id = '" ++ t ++ "' AND"

-- for to search by text string
queryWhereFind :: Request -> String
queryWhereFind req =
  case toParam req "find" of
    Nothing -> ""
    Just t ->
      " (array_position " ++
      "(" ++
      array ++
      ", '" ++
      t ++
      "') IS NOT NULL OR" ++
      " t_content LIKE '%" ++
      t ++
      "%' OR" ++
      " title LIKE '%" ++
      t ++
      "%' OR" ++
      " user_name LIKE '%" ++
      t ++ "%' OR" ++ " category_name LIKE '%" ++ t ++ "%') AND"
  where
    array = " ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id)"

-- part of the querry for SELECT post ORDER BY
queryOrder :: Request -> String
queryOrder req =
  case toParam req "order" of
    Nothing -> ""
    Just str ->
      if str == ""
        then ""
        else init (foldl addBy " ORDER BY" $ toListString str)
  where
    addBy acc x =
      case x of
        "date" -> acc ++ " draft_date,"
        "author" -> acc ++ " user_name,"
        "category" -> acc ++ " category_name,"
        "photo" ->
          acc ++ " (SELECT count (*) FROM getphotos WHERE d_id = draft_id),"
        _ -> acc

newPagination :: Pool Connection -> String -> Integer -> String -> IO ()
newPagination pool model user_id halfQuery = do
  _ <-
    liftIO $
    execSqlT
      pool
      [show user_id, model]
      "DELETE FROM pagination WHERE user_id=? AND model = ?"
  _ <-
    liftIO $
    execSqlT
      pool
      [show user_id, model, halfQuery]
      "INSERT INTO pagination (user_id, model, part_query) VALUES(?,?,?)"
  return ()

continuePagination :: Pool Connection -> String -> Integer -> IO String
continuePagination pool model user_id = do
  res <-
    liftIO $
    fetch
      pool
      [show user_id, model]
      "SELECT part_query FROM pagination WHERE user_id =? AND model =?"
  return $ pass res
  where
    pass [Only pq] = pq
    pass _ = ""

fromPhotoId :: Integer -> String
fromPhotoId i = "http://localhost:3000/photo/" ++ show i

toListString :: String -> [String]
toListString arraySql =
  words
    [ if x == ','
      then ' '
      else x
    | x <- arraySql
    , x /= '{' && x /= '}' && x /= '[' && x /= ']'
    ]

toListInteger :: String -> [Integer]
toListInteger arraySql = read $ init ('[' : tail arraySql) ++ "]"

read' :: String -> Integer
read' i =
  if all isDigit i
    then read i :: Integer
    else 1

--------------------------------------------------------------------------------
-- Utilities for interacting with the DB.
-- No transactions.
--
-- Accepts arguments
fetch :: (FromRow r, ToRow q) => Pool Connection -> q -> Query -> IO [r]
fetch pool args sql = withResource pool retrieve
  where
    retrieve conn =
      query conn sql args `catches`
      [ Handler (\(ex :: SqlError) -> handleSql ex)
      , Handler (\(ex :: ResultError) -> handleSql ex)
      ]
    handleSql _ = do
      return []

-- No arguments -- just pure sql
fetchSimple :: FromRow r => Pool Connection -> Query -> IO [r]
fetchSimple pool sql = withResource pool retrieve
  where
    retrieve conn =
      query_ conn sql `catches`
      [ Handler (\(ex :: SqlError) -> handleSql ex)
      , Handler (\(ex :: ResultError) -> handleSql ex)
      ]
    handleSql _ = do
      return []

-- Update database
execSqlT :: ToRow q => Pool Connection -> q -> Query -> IO Int64
execSqlT pool args sql = withResource pool ins
  where
    ins conn = withTransaction conn $ execute conn sql args
--------------------------------------------------------------------------------
-- Update database
-- execSql :: ToRow q => Pool Connection -> q -> Query -> IO Int64
-- execSql pool args sql = withResource pool ins
-- where ins conn = execute conn sql args
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
