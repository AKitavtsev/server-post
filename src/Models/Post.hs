{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Post
  ( Post(..)
  , Categories(..)
  , Comment(..)
  , User(..)
  ) where

import Models.Author (AuthorsDetails(..))

import Data.Aeson
import GHC.Generics

import qualified Data.Text as T

data Categories =
  Categories
    { category :: String
    , subсategories :: [String]
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data Post =
  Post
    { id :: Integer
    , title :: String
    , c_date :: String
    , author :: AuthorsDetails
    , categories :: Categories
    , tags :: [String]
    , photo :: String
    , photos :: [String]
    , text_content :: T.Text
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data Comment =
  Comment
    { c_data :: String
    , author :: User
    , comment :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data User =
  User
    { name :: String
    , surname :: String
    , avatar :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)
