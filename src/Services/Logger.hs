{-# LANGUAGE RankNTypes #-}

module Services.Logger
  ( Handle(..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  ) where

import Services.Types (Priority(..))

data Handle m =
  Handle
    {logPriority :: Monad m => Priority -> String -> m ()}

logDebug, logInfo, logWarning, logError :: Monad m => Handle m -> String -> m ()
logDebug = (`logPriority` DEBUG)

logInfo = (`logPriority` INFO)

logWarning = (`logPriority` WARN)

logError = (`logPriority` ERROR)
