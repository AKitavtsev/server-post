{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Author
  ( ForCreateAuthor(..)
  , AuthorsDetails(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics

data ForCreateAuthor =
  ForCreateAuthor
    { author_id :: Integer
    , description :: T.Text
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data AuthorsDetails =
  AuthorsDetails
    { name :: String
    , surname :: String
    , description :: T.Text
    , avatar :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)
