module Services.Logger
  ( Handle(..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  ) where

import Config (Config(..))
import Services.Types (Priority(..))

data Monad m => Handle m =
  Handle
    { config :: Config
    , logPriority :: Priority -> String -> m ()
    }

logDebug, logInfo, logWarning, logError :: Monad m => Handle m -> String -> m ()
logDebug = (`logPriority` DEBUG)

logInfo = (`logPriority` INFO)

logWarning = (`logPriority` WARN)

logError = (`logPriority` ERROR)
