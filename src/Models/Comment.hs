{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Models.Comment
  ( CommentIn(..)
  , CommentID(..)
  ) where

import Data.Aeson
import GHC.Generics

data CommentIn =
  CommentIn
    { post_id :: Integer
    , comment :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype CommentID =
  CommentID
    { id :: Integer
    }
  deriving (Show, Generic, FromJSON, ToJSON)
