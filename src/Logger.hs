
module Logger
  ( -- * Abstract handles
    Handle(..)

    -- * Priorities and convenience functions
  , Priority(..)
    -- | The following functions simplify logging at a fixed priority level.
  , logDebug
  , logInfo
  , logWarning
  , logError
  ) where


data Priority
    = Debug    -- ^ Debug messages
    | Info     -- ^ Notable information that requires no immediate action.
    | Warning  -- ^ Something is probably wrong, and we should investigate.
    | Error    -- ^ Something is wrong and immediate action is required.
    deriving (Eq, Ord, Show)

newtype Handle = Handle
    { logPriority :: Priority -> String -> IO () }
--
logDebug, logInfo, logWarning, logError :: Handle -> String -> IO ()
logDebug   = (`logPriority` Debug)
logInfo    = (`logPriority` Info)
logWarning = (`logPriority` Warning)
logError   = (`logPriority` Error)
