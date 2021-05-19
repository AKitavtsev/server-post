module Servises.Impl.Configurator
  ( newHandle
  ) where
import Servises.Data (Priority (..))

import qualified Servises.Config as SC

import qualified Data.Configurator as C
import qualified Data.Text as T
import Servises.Data (Priority (..))

newHandle :: IO SC.Handle
newHandle = do
    return $ SC.Handle
      { SC.getConfig = getconfig }
      
getconfig fm =  do   
  conf  <- C.load [C.Optional "server.conf", C.Optional "local_server.conf"]
  case fm of
    SC.DB -> do
      name  <- C.lookupDefault "" conf (T.pack "database.name") :: IO String
      user  <- C.lookupDefault "" conf (T.pack "database.user") :: IO String    
      password <- C.lookupDefault "" conf (T.pack "database.password") :: IO String 
      return (SC.DbConfig name user password)
    SC.LOG -> do
      levelStr  <- C.lookupDefault "INFO" conf (T.pack "logger.level") :: IO String 
      let level = case levelStr of
                   "DEBUG" -> DEBUG
                   "WARN"  -> WARN
                   "INFO"  -> INFO
                   "ERROR" -> ERROR
                   _       -> INFO
      return (SC.LogConfig level)
     