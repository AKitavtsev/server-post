
module Services.Impl.PostgreSQL.CreatePool
  where

import Data.Pool
import Database.PostgreSQL.Simple
import Data.Time.Clock

import Config

newConn :: Config -> IO Connection
newConn conf =
  connect
    defaultConnectInfo
      { connectUser = user conf
      , connectPassword = password conf
      , connectDatabase = name conf
      }

createPoolPostgreSQL :: Config -> IO (Pool Connection)
createPoolPostgreSQL conf = 
  createPool
    (newConn conf)
    close
    (subpools conf)
    (fromInteger (time conf) :: NominalDiffTime)
    (max_db_resours conf)