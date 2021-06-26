{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Draft (
  Photo (..),
  DraftIn (..),  
  DraftPost (..),
  DraftUp (..),
  DraftGet (..)
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
                         , category    :: Integer
                         , tags        :: (Maybe [Integer])
                         , t_content   :: (Maybe T.Text)
                         , mainPhoto   :: Photo
                         , otherPhotos :: (Maybe [Photo])
                         } deriving (Show, Generic, FromJSON, ToJSON)
                         
data DraftUp  = DraftUp  { id_draft     :: Integer
                         , title_       :: (Maybe String)
                         , category_    :: (Maybe Integer)
                         , tags_        :: (Maybe [Integer])
                         , t_content_   :: (Maybe T.Text)
                         , mainPhoto_   :: (Maybe Integer)
                         , otherPhotos_ :: (Maybe [Integer])
                         } deriving (Show, Generic, FromJSON, ToJSON)
                         
data DraftGet  = DraftGet  { title_g       :: String
                           , c_date        :: String
                           , category_g    :: Integer
                           , tags_g        :: [Integer]
                           , mainPhoto_g   :: String
                           , otherPhotos_g :: [String]
                           , t_content_g   :: T.Text
                           } deriving (Show, Generic, FromJSON, ToJSON)

                         
data DraftPost  = DraftPost  { id     :: Integer
                             , photo  :: Integer
                             , photos :: [Integer]
                             } deriving (Show, Generic, FromJSON, ToJSON)
