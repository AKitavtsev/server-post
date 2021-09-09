{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Services.Config
  ( Handle(..)
  , Config(..)
  ) where

import Services.Types (Priority(..))

newtype Handle =
  Handle
    { getConfig :: IO Config
    }

data Config =
  Config
      { name :: !String
      , user :: !String
      , password :: !String
      , limit :: !Integer
      , level :: !Priority
      , lifetime :: !Integer
      , subpools :: !Int
      , time :: !Integer
      , resours :: !Int
      }
  deriving (Show)


