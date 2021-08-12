module Servises.Impl.StdOut (
    newHandle,
) where

import qualified Servises.Config as SC
import qualified Servises.Logger as SL

import Control.Monad (when)

newHandle :: SC.Config -> IO SL.Handle
newHandle config = do
    return $
        SL.Handle
            { SL.config = config
            , SL.logPriority = logPriority
            }
  where
    logPriority prio msg = do
        let lev = case config of
                (SC.LogConfig x) -> x
        when (prio >= lev) $ do
            putStrLn ((show prio) ++ msg)
        return ()
