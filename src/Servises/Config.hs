{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servises.Config (
    Handle (..),
    ForModule (..),
    Config (..),
    getDbConfig,
    getLogConfig,
    getTokenConfig,
    getPoolConfig,
) where

import Servises.Data (Priority (..))

data ForModule
    = DB
    | LOG
    | TOKEN
    | POOL
    deriving (Eq, Ord, Show)

newtype Handle = Handle
    {getConfig :: ForModule -> IO Config}

data Config
    = DbConfig
        { name :: !String
        , user :: !String
        , password :: !String
        , limit :: !Integer
        }
    | LogConfig
        { level :: !Priority
        }
    | TokenConfig
        { lifetime :: !Integer
        }
    | PoolConfig
        { subpools :: !Int
        , time :: !Integer
        , resours :: !Int
        }
    deriving (Show)

getDbConfig, getLogConfig, getTokenConfig, getPoolConfig :: Handle -> IO Config
getDbConfig = (`getConfig` DB)
getLogConfig = (`getConfig` LOG)
getTokenConfig = (`getConfig` TOKEN)
getPoolConfig = (`getConfig` POOL)
