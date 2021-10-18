module Config
  where

import Services.Types (Priority(..))

import qualified Data.Configurator as C
import qualified Data.Text as T

data Config =
  Config
      { name :: !String
      , user :: !String
      , password :: !String
      , limit :: !Integer
      , level :: !Priority
      , lifetime :: !Integer
      , subpools :: !Int
      , time :: !Integer
      , max_db_resours :: !Int
      , host :: !String
      , port :: !Int
      }
  deriving (Show)

getConfig :: IO Config
getConfig = do
  conf <- C.load [C.Optional "server.conf", C.Optional "local_server.conf"]
  name' <- C.lookupDefault "" conf (T.pack "database.name") :: IO String
  user' <- C.lookupDefault "" conf (T.pack "database.user") :: IO String
  password' <- C.lookupDefault "" conf (T.pack "database.password") :: IO String
  limit' <- C.lookupDefault 1 conf (T.pack "database.limit") :: IO Integer
  levelStr <- C.lookupDefault "INFO" conf (T.pack "logger.loggLevel") :: IO String
  let level' = case levelStr of
                "DEBUG" -> DEBUG
                "WARN" -> WARN
                "INFO" -> INFO
                "ERROR" -> ERROR
                _ -> INFO
  lifetime' <- C.lookupDefault 86400 conf (T.pack "token.lifetime") :: IO Integer
  subpools' <- C.lookupDefault 1 conf (T.pack "poll.subpools") :: IO Int
  time' <- C.lookupDefault 40 conf (T.pack "poll.time") :: IO Integer
  resources' <- C.lookupDefault 10 conf (T.pack "poll.max_db_resours") :: IO Int
  host' <- C.lookupDefault "http://localhost" conf (T.pack "host_port.host") :: IO String
  port' <- C.lookupDefault 3000 conf (T.pack "host_port.port") :: IO Int
  return (Config name' user' password' limit' level' lifetime' subpools' time' resources' host' port')
