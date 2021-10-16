{-# LANGUAGE OverloadedStrings #-}

module Main where

import Router (routes)
import Config
import Services.Db (close, newConn)
import Services.Impl.PostgreSQL.Migrations
import Services.Logger

import qualified Services.Impl.MD5 as ST
import qualified Services.Impl.PostgreSQL as SB
import qualified Services.Impl.StdOut as SL

import Data.Pool (createPool)
import Data.Time.Clock
import Control.Monad (when)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

main :: IO ()
main = do
  conf <- getConfig
  hDb  <- SB.newHandle conf
  conn <- newConn hDb conf
  pool <-
    createPool
      (newConn hDb conf)
      (close hDb)
      (subpools conf)
      (fromInteger (time conf) :: NominalDiffTime)
      (max_db_resours conf)
  hLogger <- SL.newHandle conf
  hToken <- ST.newHandle conf    
  args <- getArgs
  when (args == ["migration"]) $ 
    runMigrations hLogger conn pool "sql"
  logInfo hLogger ("  Listen port " ++ show (port conf))
  run (port conf) (routes pool hLogger hToken hDb)
