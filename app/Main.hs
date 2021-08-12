{-# LANGUAGE OverloadedStrings #-}

module Main where

import Router (routes)
import Servises.Db (newConn, close)
import Servises.Logger
import Servises.Config

import qualified Servises.Impl.StdOut as SL
import qualified Servises.Impl.Configurator as SC
import qualified Servises.Impl.MD5 as ST
import qualified Servises.Impl.PostgreSQL as SB

import Data.Pool (Pool, createPool, withResource)
import Data.Time.Clock
import Control.Monad (when)
import System.Environment
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  hConfig    <- SC.newHandle  
  dbConf     <- getDbConfig hConfig
  poolConfig <- getPoolConfig hConfig  
  hDb        <- SB.newHandle dbConf  
  conn       <- newConn hDb dbConf  
  pool       <- createPool (newConn hDb dbConf)
                           (close hDb)  
                           (subpools poolConfig)
                           (fromInteger (time poolConfig) :: NominalDiffTime)
                           (resours poolConfig)
  logConf     <- getLogConfig hConfig 
  hLogger     <- SL.newHandle logConf  
  tokenConfig <- getTokenConfig hConfig
  hToken      <- ST.newHandle tokenConfig 
  
  logInfo hLogger "  Listen port 3000"
  
  run 3000 (routes conn pool hLogger hToken hDb)
    