{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servises.Config
import Db
import Servises.Logger
import Migrations

import qualified Servises.Impl.StdOut as LS
import qualified Servises.Impl.Configurator as CC

import qualified Controllers.Users
import qualified Controllers.Images
import qualified Controllers.Token
import FromRequest (toPath)
-- вроде base
-- import Control.Concurrent.STM
-- import Control.Monad.Reader
-- import Network.Wai.Middleware.RequestLogger
-- import System.Environment
import Data.Pool(Pool, createPool, withResource)
import Database.PostgreSQL.Simple
import Control.Monad (when)
import System.Environment
import Network.Wai
import Network.Wai.Handler.Warp

import Network.HTTP.Types




-- import qualified Data.Configurator as C
-- import qualified Data.Text as T


-- import Core

-- import qualified Controllers.Accounts
-- import qualified Controllers.AccountResources
-- import qualified Controllers.Operations
-- import qualified Persistence.Database as DB

-- start the server and load the routes from the controllers
main :: IO ()
main = do

  -- conf  <- C.load [C.Optional "server.conf", C.Optional "local_server.conf"]    
  hConfig <- CC.newHandle
  dbConf <- getDbConfig hConfig
  conn <- newConn dbConf  
  pool <- createPool (newConn dbConf) close 1 40 10  
  mig <- getArgs
  when (mig == ["-m"]) $ do
    begin conn
    runMigrations pool "sql"
    commit  conn
  hLogger <- LS.newHandle hConfig
  logInfo hLogger "  Listen port 3000"
  run 3000 (routes pool)
    
routes pool req respond  = do
    case toPath req of
        "user"  -> do
          Controllers.Users.routes pool req respond
        "token" -> do
          Controllers.Token.routes pool req respond
        "image" -> do
          Controllers.Images.routes pool req respond
        _       -> do
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""

  
-- routes pool = do


  -- Controllers.Users.routes pool
  -- Controllers.AccountResources.routes
  -- Controllers.Operations.routes


