
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
    {logPriority :: Priority -> String -> m ()}

logDebug, logInfo, logWarning, logError ::Handle m -> String -> m ()
logDebug = (`logPriority` DEBUG)

logInfo = (`logPriority` INFO)

logWarning = (`logPriority` WARN)

logError = (`logPriority` ERROR)
