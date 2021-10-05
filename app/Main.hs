{-# LANGUAGE OverloadedStrings #-}

module Main where

import Router (routes)
import Config
import Services.Db (close, newConn)
import Services.Logger

import qualified Services.Impl.MD5 as ST
import qualified Services.Impl.PostgreSQL as SB
import qualified Services.Impl.StdOut as SL

import Data.Pool (createPool)
import Data.Time.Clock
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  conf <- getConfig
  hDb <- SB.newHandle conf
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
  logInfo hLogger "  Listen port 3000"
  run 3000 (routes conn pool hLogger hToken hDb)
