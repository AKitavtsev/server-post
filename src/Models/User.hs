{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models.User
  ( Avatar(..)
  , ForCreateUser(..)
  , ForShowUser(..)
  , TokenForUser(..)
  ) where

import Data.Aeson
import GHC.Generics

-- represents a user
data Avatar =
  Avatar
    { image :: String
    , typ :: String
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ForCreateUser =
  ForCreateUser
    { name :: String
    , surname :: String
    , avatar :: Maybe Avatar
    , login :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data ForShowUser =
  ForShowUser
    { name_ :: String
    , surname_ :: String
    , avatar_ :: String
    , login_ :: String
    , c_data :: String
    , admin :: Bool
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data TokenForUser =
  TokenForUser
    { id :: Integer
    , token :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)
