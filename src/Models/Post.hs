{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Post (
  Post (..),
  ) where

import  GHC.Generics
import Data.Aeson

import qualified Data.Text as T

             
data Post  = Post  { id          :: Integer
                   , title       :: String
                   , c_date      :: String
                   , author      :: Integer
                   , category    :: Integer
                   , tags        :: [Integer]
                   , photo       :: Integer
                   , photos      :: [Integer]
                   , t_content   :: T.Text
                   } deriving (Show, Generic, FromJSON, ToJSON)

data Posts = Posts [Post] deriving (Show, Generic, FromJSON, ToJSON)