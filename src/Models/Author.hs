{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Author (
  Author (..),
  AuthorOut (..)
  ) where

import  GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T

data Author  = Author  { author_id      :: Integer
                       , description    :: T.Text  
                       } deriving (Show, Generic, FromJSON, ToJSON)
                       
data AuthorOut = AuthorOut { name        :: String
                           , surname     :: String
                           , description :: T.Text
                           } deriving (Show, Generic, FromJSON, ToJSON)
