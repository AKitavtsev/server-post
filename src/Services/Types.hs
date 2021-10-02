module Services.Types
  ( Priority(..)
  ) where

data Priority
  = DEBUG
  | INFO
  | WARN
  | ERROR
  deriving (Eq, Ord, Show)
