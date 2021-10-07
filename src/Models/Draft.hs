{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Draft
  ( Photo(..)
  , RawDraft(..)
  , FoundTagsAndPhotosForDraft(..)
  , ForUpdateDraft(..)
  , ForShowDraft(..)
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

data RawDraft =
  RawDraft
    { title :: String
    , category :: Integer
    , tags :: [Integer]
    , t_content :: T.Text
    , main_photo :: Integer
    , other_photos :: [Integer]
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data ForUpdateDraft =
  ForUpdateDraft
    { id_draft :: Integer
    , new_title :: Maybe String
    , new_category :: Maybe Integer
    , new_tags :: Maybe [Integer]
    , new_content :: Maybe T.Text
    , new_main_photo :: Maybe Integer
    , new_other_photos :: Maybe [Integer]
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data ForShowDraft =
  ForShowDraft
    { title_g :: String
    , c_date :: String
    , category_g :: Integer
    , tags_g :: [Integer]
    , main_photo_g :: String
    , other_photos_g :: [String]
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
