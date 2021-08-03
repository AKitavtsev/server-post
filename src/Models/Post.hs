{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Post (
  Post (..),
  Categories (..)
  ) where

import Models.Author (AuthorOut (..))

import GHC.Generics
import Data.Aeson

import qualified Data.Text as T


data Categories = Categories { category       :: String
                             , subCategories  :: [String]
                             } deriving (Show, Generic, FromJSON, ToJSON)                            
data Post  = Post  { id             :: Integer
                   , title          :: String
                   , c_date         :: String
                   , author         :: AuthorOut 
                   , categories     :: Categories
                   , tags           :: [String]
                   , photo          :: String
                   , photos         :: [String]
                   , text_content   :: T.Text
                   } deriving (Show, Generic, FromJSON, ToJSON)