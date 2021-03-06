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
    , creation_date :: String
    , author :: AuthorsDetails
    , categories :: Categories
    , tags :: [String]
    , photo :: String
    , photos :: [String]
    , text_content :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data Comment =
  Comment
    { creation_date :: String
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
