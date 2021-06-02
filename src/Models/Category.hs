{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Category (
  Category (..)
  ) where

import  GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data Category  = Category  { name     :: String 
                           , id_owner :: Maybe Integer                     
                           } deriving (Show, Generic, FromJSON, ToJSON)
                       
