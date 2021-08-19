{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Author
  ( Author(..)
  , AuthorOut(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics

data Author =
  Author
    { author_id :: Integer
    , description :: T.Text
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data AuthorOut =
  AuthorOut
    { name :: String
    , surname :: String
    , description :: T.Text
    , avatar :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)
