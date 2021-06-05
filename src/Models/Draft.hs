{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Draft (
  Photo (..),
  DraftIn (..),
  DraftPost (..)
  ) where

import Data.Aeson
import GHC.Generics

import qualified Data.Text as T


data Photo  = Photo { image :: String
                    , typ   :: String
                    } deriving (Eq, Show, Generic, FromJSON, ToJSON)
             
data DraftIn  = DraftIn  { title       :: String
                         , category    :: Integer
                         , tags        :: [Integer]
                         , t_content   :: T.Text
                         , mainPhoto   :: Photo
                         , otherPhotos :: [Photo]
                         } deriving (Show, Generic, FromJSON, ToJSON)

data DraftPost  = DraftPost  { id     :: Integer
                             , photo  :: Integer
                             , photos :: [Integer]
                             } deriving (Show, Generic, FromJSON, ToJSON)
