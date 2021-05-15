{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.User (
  Avatar (..),
  UserIn (..),
  UserOut (..),
  UserID (..)
  ) where

import  GHC.Generics
import Data.Aeson
-- import Data.UUID

-- represents a user
data Avatar  = Avatar { image :: String
                      , typ   :: String
                      } deriving (Eq, Show, Generic, FromJSON, ToJSON)
             
data UserIn  = UserIn  { name     :: String
                       , surname  :: String
                       , avatar   :: Maybe Avatar
                       , login    :: String
                       , password :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)

data UserOut = UserOut { user_id  :: Integer
                       , name_    :: String
                       , surname_ :: String
                       , avatar_  :: String
                       , login_   :: String
                       , c_data   :: String
                       , admin    :: Bool
                       } deriving (Show, Generic, FromJSON, ToJSON)

data UserID  = UserID  { user_id_ :: Integer
                       , token    :: String   
                       } deriving (Show, Generic, FromJSON, ToJSON)
