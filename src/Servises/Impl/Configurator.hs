module Servises.Impl.Configurator
  ( newHandle
  ) where
  
import Servises.Data (Priority (..))

import qualified Servises.Config as SC
import qualified Data.Configurator as C
import qualified Data.Text as T


newHandle :: IO SC.Handle
newHandle = do
    return $ SC.Handle
      { SC.getConfig = getconfig }
      
getconfig fm =  do   
  conf  <- C.load [C.Required "server.conf"]
  case fm of
    SC.DB -> do
      name     <- C.lookupDefault "" conf (T.pack "database.name") :: IO String
      user     <- C.lookupDefault "" conf (T.pack "database.user") :: IO String    
      password <- C.lookupDefault "" conf (T.pack "database.password") :: IO String 
      limit    <- C.lookupDefault 1 conf (T.pack "database.limit") :: IO Integer
      return (SC.DbConfig name user password limit)
    SC.LOG -> do
      levelStr  <- C.lookupDefault "INFO" conf (T.pack "logger.level") :: IO String 
      let level = case levelStr of
                   "DEBUG" -> DEBUG
                   "WARN"  -> WARN
                   "INFO"  -> INFO
                   "ERROR" -> ERROR
                   _       -> INFO
      return (SC.LogConfig level)
    SC.TOKEN -> do
      lifetime <- C.lookupDefault 86400 conf (T.pack "token.lifetime") :: IO Integer
      return (SC.TokenConfig lifetime)
    SC.POOL -> do
      subpools <- C.lookupDefault 1 conf (T.pack "poll.subpools") :: IO Int
      time     <- C.lookupDefault 40 conf (T.pack "poll.time") :: IO Integer
      resours  <- C.lookupDefault 10 conf (T.pack "poll.resours") :: IO Int
      return (SC.PoolConfig subpools time resours)
       