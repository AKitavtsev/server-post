{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Post (
  Post (..),
  ) where

import Models.Author (AuthorOut (..))

import GHC.Generics
import Data.Aeson

import qualified Data.Text as T


-- data Post  = Post  { id    :: Integer
                   -- , title       :: String
                   -- , c_date      :: String
                   -- , author_name :: String
                   -- , surname     :: String
                   -- , description :: String
                   -- , category    :: Integer
                   -- , tags        :: [Integer]
                   -- , photo       :: String
                   -- , photos      :: [String]
                   -- , text_content   :: T.Text
                   -- } deriving (Show, Generic, FromJSON, ToJSON)
                   
data Post  = Post  { id    :: Integer
                   , title       :: String
                   , c_date      :: String
                   , author      :: AuthorOut 
                   , category    :: String
                   , tags        :: [Integer]
                   , photo       :: String
                   , photos      :: [String]
                   , text_content   :: T.Text
                   } deriving (Show, Generic, FromJSON, ToJSON)