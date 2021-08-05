{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Servises.Config
  ( Handle(..)
  , ForModule (..)
  , Config (..)
  , getDbConfig 
  , getLogConfig
  , getTokenConfig
  , getPoolConfig
  ) where

import Servises.Data (Priority (..))

-- import Control.Monad (when)
-- import Data.List (all)
-- import Data.Char
-- import System.Exit
 
-- import qualified Data.Configurator as C
-- import qualified Data.Configurator.Types as TC
-- import qualified Data.Text as T

data ForModule = DB 
               | LOG 
               | TOKEN 
               | POOL 
               deriving (Eq, Ord, Show)

newtype Handle = Handle
    { getConfig :: ForModule -> IO Config}  

data Config = DbConfig    { name      :: !String
                          , user      :: !String
                          , password  :: !String
                          , limit     :: !Integer
                          }
            | LogConfig   { level     :: !Priority
                          }
            | TokenConfig { lifetime  :: !Integer
                          }
            | PoolConfig  { subpools  :: !Int
                          , time      :: !Integer
                          , resours   :: !Int
                          }
             deriving (Show)



getDbConfig, getLogConfig, getTokenConfig :: Handle -> IO Config
getDbConfig    = (`getConfig` DB)
getLogConfig   = (`getConfig` LOG)
getTokenConfig = (`getConfig` TOKEN)
getPoolConfig  = (`getConfig` POOL)

