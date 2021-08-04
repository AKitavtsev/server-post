{-# LANGUAGE OverloadedStrings #-}

module Servises.Impl.PostgreSQL.Internal 
  where
  
import Servises.Impl.PostgreSQL.Pagination

import Database.PostgreSQL.Simple
import Data.Pool (Pool (..))
import Data.Char (isDigit) 
import Network.Wai (Request (..))
import Control.Applicative ((<|>))

-- import qualified Data.ByteString.Char8 as BC

import FromRequest

queryWhereOrder :: Pool Connection -> Request -> Integer -> IO String
queryWhereOrder pool req id_user = 
  case toParam req "page" of
    Nothing    -> do
      newPagination pool  "post" id_user ((queryWhere req) ++ (queryOrder req))
      return ((queryWhere req) ++ (queryOrder req) ++ " LIMIT 1")
    Just page  -> do
--     
      q <- continuePagination pool "post" id_user
      return  (q ++ "LIMIT 1 OFFSET 2")    
  where read' i = if (all isDigit i) then read i ::Integer else 1
          
queryWhere :: Request -> String
queryWhere req = case qw of
                   "" -> ""
                   _  -> " WHERE" ++ reverse (drop 4 $ reverse qw)
  where qw = (queryWhereTag req)           
          ++ (queryWhereTitle req)
          ++ (queryWhereText req)
          ++ (queryWhereDate req)
          ++ (queryWhereAuthor req)
          ++ (queryWhereCategory req)
          ++ (queryWhereFind req)

queryWhereTag :: Request -> String
queryWhereTag req = case tag <|> tagsIn <|> tagsAll of
                       Nothing -> ""
                       Just str -> str
  where
    array = " ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id)"
    tag     = case toParam req "tag" of
                Nothing -> Nothing 
                Just t  -> Just (" array_position " ++ "(" ++ 
                                array ++ "," ++ t ++ ") IS NOT NULL AND")
    tagsIn  = case toParam req "tags_in" of
                Nothing -> Nothing
                Just t  -> Just (" " ++ array ++ " && ARRAY " ++ t ++ " AND")
    tagsAll = case toParam req "tags_all" of
                Nothing -> Nothing
                Just t  -> Just (" " ++ array ++ " @> ARRAY " ++ t ++ " AND")
                
queryWhereTitle :: Request -> String
queryWhereTitle req =
  case toParam req "title" of
                Nothing -> "" 
                Just t  -> " title LIKE '%" ++ t ++ "%' AND"

queryWhereText :: Request -> String
queryWhereText req =
  case toParam req "text" of
                Nothing -> "" 
                Just t  -> " t_content LIKE '%" ++ t ++ "%' AND"

queryWhereDate :: Request -> String
queryWhereDate req = case dateAT <|> dateLT <|> dateGT  of
                       Nothing  -> ""
                       Just str -> str                      
  where
    beg = " draft_date :: date "
    dateAT = case toParam req "created_at" of
                Nothing -> Nothing 
                Just t  -> Just (beg ++ "='" ++ t ++ "' AND")
    dateLT = case toParam req "created_lt" of
                Nothing -> Nothing 
                Just t  -> Just (beg ++ "<'" ++ t ++ "' AND")
    dateGT = case toParam req "created_gt" of
                Nothing -> Nothing 
                Just t  -> Just (beg ++ ">'" ++ t ++ "' AND")
                
queryWhereAuthor :: Request -> String
queryWhereAuthor req =
  case toParam req "name" of
                Nothing -> "" 
                Just t  -> " user_name = '" ++ t ++ "' AND"
                
queryWhereCategory :: Request -> String
queryWhereCategory req =
  case toParam req "category" of
                Nothing -> "" 
                Just t  -> " category_id = '" ++ t ++ "' AND"

queryWhereFind :: Request -> String
queryWhereFind req =

  case toParam req "find" of
                Nothing -> "" 
                Just t  -> " (array_position " ++ "(" ++ 
                             array ++ ", '" ++ t ++ "') IS NOT NULL OR"
                        ++ " t_content LIKE '%" ++ t ++ "%' OR"
                        ++ " title LIKE '%" ++ t ++ "%' OR"
                        ++ " user_name LIKE '%" ++ t ++ "%' OR"
                        ++ " category_name LIKE '%" ++ t ++ "%') AND"
  where  array = " ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id)"
                       
queryOrder :: Request -> String
queryOrder req = 
  case toParam req "order" of
      Nothing  -> ""
      Just str -> if str == "" then "" 
                  else init (foldl addBy " ORDER BY" $ toListString str)
  where
    addBy acc x = 
      case x of
        "date"        -> acc ++ " draft_date,"
        "author"      -> acc ++ " user_name,"
        "category"    -> acc ++ " category_name,"
        "photo"       -> acc ++ " (SELECT count (*) FROM getphotos WHERE d_id = draft_id),"
        _             -> acc
       
fromPhotoId :: Integer -> String      
fromPhotoId id = "http://localhost:3000/photo/" ++ (show id)

toListString :: String -> [String]
toListString arraySql = words [if x == ',' then ' ' else x|
                               x <- arraySql, 
                               x /= '{' && x /= '}' && x /= '[' && x /= ']']
                               
toListInteger :: String -> [Integer]
toListInteger arraySql = read $ init ('[': (tail arraySql)) ++ "]"
      