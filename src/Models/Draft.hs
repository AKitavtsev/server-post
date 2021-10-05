{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Draft
  ( Photo(..)
  , ForCreateDraft(..)
  , FoundTagsAndPhotosForDraft(..)
  , ForUpdateDraft(..)
  , ForShowDraf(..)
  ) where

import Data.Aeson
import GHC.Generics

import qualified Data.Text as T

data Photo =
  Photo
    { image :: String
    , typ :: String
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ForCreateDraft =
  ForCreateDraft
    { title :: String
    , category :: Integer
    , tags :: [Integer]
    , t_content :: T.Text
    , mainPhoto :: Integer
    , otherPhotos :: [Integer]
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data ForUpdateDraft =
  ForUpdateDraft
    { id_draft :: Integer
    , newTitle :: Maybe String
    , newCategory :: Maybe Integer
    , newTags :: Maybe [Integer]
    , newContent :: Maybe T.Text
    , newMainPhoto :: Maybe Integer
    , newOtherPhotos :: Maybe [Integer]
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data ForShowDraf =
  ForShowDraf
    { title_g :: String
    , c_date :: String
    , category_g :: Integer
    , tags_g :: [Integer]
    , mainPhoto_g :: String
    , otherPhotos_g :: [String]
    , t_content_g :: T.Text
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data FoundTagsAndPhotosForDraft =
  FoundTagsAndPhotosForDraft
    { id :: Integer
    , tags' :: [Integer]
    , photos :: [Integer]
    }
  deriving (Show, Generic, FromJSON, ToJSON)
