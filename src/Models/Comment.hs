{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.Comment (
  CommentIn (..),
  CommentOut (..),
  CommentID (..)
  ) where

import  GHC.Generics
import Data.Aeson

             
data CommentIn  = CommentIn  { post_id  :: Integer
                             , comment  :: String
                             } deriving (Show, Generic, FromJSON, ToJSON)

data CommentOut = CommentOut { id       :: Integer
                             , c_date   :: String
                             , post_id  :: Integer
                             , comment  :: String
                             } deriving (Show, Generic, FromJSON, ToJSON)

data CommentID = CommentID   { id :: Integer
                             } deriving (Show, Generic, FromJSON, ToJSON)

