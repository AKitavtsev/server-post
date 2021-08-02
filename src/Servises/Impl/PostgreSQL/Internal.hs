{-# LANGUAGE OverloadedStrings #-}

module Servises.Impl.PostgreSQL.Internal 
  where

import Network.Wai (Request (..))
import Control.Applicative ((<|>))

-- import qualified Data.ByteString.Char8 as BC

import FromRequest

queryWhere :: Request -> String
queryWhere req = case qw of
                   "" -> ""
                   _  -> " WHERE" ++ reverse (drop 4 $ reverse qw)
  where qw = (queryWhereTag req)           
          ++ (queryWhereTitle req)
          ++ (queryWhereText req)

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
