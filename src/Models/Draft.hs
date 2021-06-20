{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Draft (
  Photo (..),
  DraftIn (..),  
  DraftPost (..),
  DraftUp (..)
  ) where

import Data.Aeson
import GHC.Generics

import qualified Data.Text as T


data Photo  = Photo { image :: String
                    , typ   :: String
                    } deriving (Eq, Show, Generic, FromJSON, ToJSON)
             
-- data DraftIn  = DraftIn  { id_draft    :: (Maybe Integer)
                         -- , title       :: String
                         -- , category    :: Integer
                         -- , tags        :: [Integer]
                         -- , t_content   :: T.Text
                         -- , mainPhoto   :: Photo
                         -- , otherPhotos :: [Photo]
                         -- } deriving (Show, Generic, FromJSON, ToJSON)

-- data DraftIn  = DraftIn  { id_draft    :: (Maybe Integer)
                         -- , title       :: (Maybe String)
                         -- , category    :: (Maybe Integer)
                         -- , tags        :: (Maybe [Integer])
                         -- , t_content   :: (Maybe T.Text)
                         -- , mainPhoto   :: (Maybe Photo)
                         -- , otherPhotos :: (Maybe [Photo])
                         -- } deriving (Show, Generic, FromJSON, ToJSON)

data DraftIn  = DraftIn  { title       :: (Maybe String)
                         , category    :: Integer  -- прoверить с помощью Exeption
                         , tags        :: (Maybe [Integer]) -- проверить и подменить checkAvailabilityTags
                         , t_content   :: (Maybe T.Text)
                         , mainPhoto   :: Photo
                         , otherPhotos :: (Maybe [Photo])
                         } deriving (Show, Generic, FromJSON, ToJSON)
                         
data DraftUp  = DraftUp  { id_draft     :: Integer -- проверить с помощью Exeption
                         , title_        :: (Maybe String)
                         , category_    :: (Maybe Integer) -- проверить с помощью Exeption
                         , tags_        :: (Maybe [Integer]) -- проверить и подменить checkAvailabilityTags
                         , t_content_   :: (Maybe T.Text)
                         , mainPhoto_   :: (Maybe Integer) -- проверить с помощью ???
                         , otherPhotos_ :: (Maybe [Integer]) -- проверить и подменить checkAvailabilityPhotos
                         } deriving (Show, Generic, FromJSON, ToJSON)
                         
data DraftPost  = DraftPost  { id     :: Integer
                             , photo  :: Integer
                             , photos :: [Integer]
                             } deriving (Show, Generic, FromJSON, ToJSON)
