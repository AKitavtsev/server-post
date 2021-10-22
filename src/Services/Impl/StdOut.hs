module Services.Impl.StdOut
  ( newHandle
  ) where

import qualified Config as SC
import qualified Services.Logger as SL

import Control.Monad (when)

newHandle :: SC.Config -> IO (SL.Handle IO)
newHandle config = do
  return $ SL.Handle {SL.config = config, SL.logPriority = logPriority}
  where
    logPriority prio msg = do
      when (prio >= SC.level config) $ do putStrLn (show prio ++ msg)
