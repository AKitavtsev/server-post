{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config
    ( DbConfig (..)
    , getDbConfig 
    )
        where

-- import Control.Monad (when)
-- import Data.List (all)
-- import Data.Char
-- import System.Exit
 
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as TC
import qualified Data.Text as T

    
data DbConfig = DbConfig {name     :: !String
                         ,user     :: !String
                         ,password :: !String
                         } deriving (Show)
    
getDbConfig :: TC.Config -> IO DbConfig
getDbConfig conf = do
    -- conf  <- C.load [C.Optional "server.conf", C.Optional "local_server.conf"]    
    name  <- C.lookupDefault "" conf (T.pack "database.name") :: IO String
    user  <- C.lookupDefault "" conf (T.pack "database.user") :: IO String    
    password <- C.lookupDefault "" conf (T.pack "database.password") :: IO String 
    return (DbConfig name user password)