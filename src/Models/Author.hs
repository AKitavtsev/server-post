{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Author
  ( RawAuthor(..)
  , AuthorsDetails(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data RawAuthor =
  RawAuthor
    { author_id :: Integer
    , description :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data AuthorsDetails =
  AuthorsDetails
    { name :: String
    , surname :: String
    , description :: String
    , avatar :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)
