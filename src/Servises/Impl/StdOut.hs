module Servises.Impl.StdOut
  ( newHandle
  ) where

import qualified Servises.Logger as SL
import qualified Servises.Config as SC

import Control.Monad (when)

newHandle :: SC.Handle -> IO SL.Handle
newHandle handle = do
    config <- SC.getLogConfig handle 
    putStrLn (show config)    
    return $ SL.Handle
      { SL.config = config
      , SL.logPriority = logPriority
      -- \prio msg ->
          -- putStrLn ((show prio) ++ msg)
      }      
      where 
        logPriority prio msg = do
        -- config <- SC.getLogConfig handle
          -- when (prio >= (level (SL.config handle))) 
            putStrLn ((show prio) ++ msg ++ (show config))
          -- return ()