{-# LANGUAGE OverloadedStrings #-}

module Main where



import Config
import Router (routes)
import Services.Impl.PostgreSQL.CreatePool
import Services.Impl.PostgreSQL.Migrations
import Services.Logger

import qualified Services.Impl.MD5 as ST
import qualified Services.Impl.PostgreSQL as SB
import qualified Services.Impl.StdOut as SL

import Control.Monad (when)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import FromRequest

main :: IO ()
main = do
  conf <- getConfig
  conn <- newConn conf
  pool <- createPoolPostgreSQL conf
  hDb <- SB.newHandle conf pool
  hLogger <- SL.newHandle conf
  hToken <- ST.newHandle conf
  args <- getArgs
  when (args == ["migration"]) $ 
    runMigrations hLogger conn pool "sql"
  logInfo hLogger ("  Listen port " ++ show (port conf))
  run (port conf) (routes hLogger hToken hDb hRequstIO)
