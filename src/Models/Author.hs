{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Author
  ( RawAuthor(..)
  , AuthorsDetails(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics

data RawAuthor =
  RawAuthor
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
