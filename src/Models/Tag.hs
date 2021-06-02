{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Tag (
  Tag (..),
  ) where

import  GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data Tag = Tag {tag :: String}  
             deriving (Eq, Show, Generic, FromJSON, ToJSON)
