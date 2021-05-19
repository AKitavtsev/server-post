{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Servises.Config
  ( Handle(..)
  , ForModule (..)
  , Config (..)
  , getDbConfig 
  , getLogConfig
  ) where

import Servises.Data (Priority (..))

-- import Control.Monad (when)
-- import Data.List (all)
-- import Data.Char
-- import System.Exit
 
-- import qualified Data.Configurator as C
-- import qualified Data.Configurator.Types as TC
-- import qualified Data.Text as T

data ForModule = DB | LOG deriving (Eq, Ord, Show)

newtype Handle = Handle
    { getConfig :: ForModule -> IO Config}  

data Config = DbConfig {name     :: !String
                       ,user     :: !String
                       ,password :: !String
                       }
            | LogConfig {level :: !Priority}
            deriving (Show)



getDbConfig, getLogConfig :: Handle -> IO Config
getDbConfig  = (`getConfig` DB)
getLogConfig = (`getConfig` LOG)

