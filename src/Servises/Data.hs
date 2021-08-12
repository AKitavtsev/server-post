module Servises.Data
    (Priority (..)) where

data Priority
    = DEBUG    -- ^ Debug messages
    | INFO     -- ^ Notable information that requires no immediate action.
    | WARN     -- ^ Something is probably wrong, and we should investigate.
    | ERROR    -- ^ Something is wrong and immediate action is required.
    deriving (Eq, Ord, Show)