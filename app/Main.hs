{-# LANGUAGE OverloadedStrings #-}

module Main where

import Router (routes)
import Config
import Services.Impl.PostgreSQL.CreatePool
import Services.Impl.PostgreSQL.Migrations
import Services.Logger

import qualified Services.Impl.MD5 as ST
import qualified Services.Impl.PostgreSQL as SB
import qualified Services.Impl.StdOut as SL

import Control.Monad (when)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

main :: IO ()
main = do
  conf <- getConfig
  conn <- newConn conf
  pool <- createPoolPostgreSQL conf
  hDb  <- SB.newHandle conf pool
  hLogger <- SL.newHandle conf
  hToken <- ST.newHandle conf
  args <- getArgs
  when (args == ["migration"]) $ 
    runMigrations hLogger conn pool "sql"
  logInfo hLogger "  Listen port 3000"
  run 3000 (routes hLogger hToken hDb)
