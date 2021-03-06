{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Tag
  ( Tag(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

newtype Tag =
  Tag
    { tag :: String
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
