module Logger.Impl.StdOut
  ( newHandle
  ) where

import qualified Logger
-- import           Data.Monoid             (<>)
-- import           Control.Concurrent.MVar (newMVar, withMVar)
-- import qualified System.IO

-- | Create a new 'Logger.Handle' that logs to a 'System.IO.Handle'.
newHandle :: IO Logger.Handle
newHandle = do
    -- We use a mutex to make our logger thread-safe.
    -- (Note that we should take this mutex as an argument for maximal
    -- compositionality.)

    return $ Logger.Handle
      { Logger.logPriority = \prio msg ->
        putStrLn ((show prio) ++ msg)
      }