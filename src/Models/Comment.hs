{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Models.Comment
  ( ForCreateComment(..)
  , IdComment(..)
  ) where

import Data.Aeson
import GHC.Generics

data ForCreateComment =
  ForCreateComment
    { post_id :: Integer
    , comment :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype IdComment =
  IdComment
    { id :: Integer
    }
  deriving (Show, Generic, FromJSON, ToJSON)
