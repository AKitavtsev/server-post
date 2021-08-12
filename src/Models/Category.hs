{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Category (
    Category (..),
    Cat (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Category = Category
    { name :: String
    , id_owner :: Maybe Integer
    }
    deriving (Show, Generic, FromJSON, ToJSON)

data Cat = Cat
    { id :: Integer
    , cat :: String
    }
    deriving (Show)
