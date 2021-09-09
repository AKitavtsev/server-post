module Services.Impl.Configurator
  ( newHandle
  ) where

import Services.Types (Priority(..))

import qualified Data.Configurator as C
import qualified Data.Text as T
import qualified Services.Config as SC

newHandle :: IO SC.Handle
newHandle = do
  return $ SC.Handle {SC.getConfig = getconfig}
  where
    getconfig = do
      conf <- C.load [C.Optional "server.conf", C.Optional "local_server.conf"]
      name <- C.lookupDefault "" conf (T.pack "database.name") :: IO String
      user <- C.lookupDefault "" conf (T.pack "database.user") :: IO String
      password <- C.lookupDefault "" conf (T.pack "database.password") :: IO String
      limit <- C.lookupDefault 1 conf (T.pack "database.limit") :: IO Integer
      levelStr <-
        C.lookupDefault "INFO" conf (T.pack "logger.loggLevel") :: IO String
      let level =
            case levelStr of
              "DEBUG" -> DEBUG
              "WARN" -> WARN
              "INFO" -> INFO
              "ERROR" -> ERROR
              _ -> INFO
      lifetime <- C.lookupDefault 86400 conf (T.pack "token.lifetime") :: IO Integer
      subpools <- C.lookupDefault 1 conf (T.pack "poll.subpools") :: IO Int
      time <- C.lookupDefault 40 conf (T.pack "poll.time") :: IO Integer
      resours <- C.lookupDefault 10 conf (T.pack "poll.resours") :: IO Int
      return (SC.Config name user password limit
                        level
                        lifetime
                        subpools time resours)
