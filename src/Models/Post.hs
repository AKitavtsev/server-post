{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Post (
  Post (..),
  ) where

import  GHC.Generics
import Data.Aeson

import qualified Data.Text as T

-- data DraftGet  = DraftGet  { title_g       :: String
                           -- , c_date        :: String
                           -- , category_g    :: (Integer, String)
                           -- , tags_g        :: [String]
                           -- , mainPhoto_g   :: String
                           -- , otherPhotos_g :: [String]
                           -- , t_content_g   :: T.Text
                           -- } deriving (Show, Generic, FromJSON, ToJSON)
             
-- data Post  = Post  { id          :: Integer
                   -- , title       :: String
                   -- , c_date      :: String
                   -- , author      :: Integer
                   -- , category    :: Integer
                   -- , tags        :: [Integer]
                   -- , photo       :: Integer
                   -- , photos      :: [Integer]
                   -- , t_content   :: T.Text
                   -- } deriving (Show, Generic, FromJSON, ToJSON)
                   
data Post  = Post  { id          :: Integer
                   , title       :: String
                   , c_date      :: String
                   , name        :: String
                   , surname     :: String
                   , category    :: String
                   , tags        :: [String]
                   , photo       :: String
                   , photos      :: [String]
                   , t_content   :: T.Text
                   } deriving (Show, Generic, FromJSON, ToJSON)
                   
data Posts = Posts [Post] deriving (Show, Generic, FromJSON, ToJSON)