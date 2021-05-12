{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Migrations (runMigrations)
    where

import Control.Monad (forM_, void, when)
-- import Data.Function 
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (..))
import Data.Pool
import System.Directory

import qualified Data.ByteString as BS 
import qualified Data.List as L

  
runMigrations :: Pool Connection -> FilePath -> IO ()
runMigrations pool dir = do
    fn <- scriptsInDirectory dir 
    withResource pool $ \conn -> do
      forM_ fn (executeMigration conn False) 
          
executeMigration :: Connection -> Bool -> FilePath -> IO ()
executeMigration con verbose fileName  = do
            content <- BS.readFile fileName
            void $ execute_ con (Query content)
            when verbose $ putStrLn $ "Execute:\t" ++ fileName
            return () 

-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: FilePath -> IO [FilePath]
scriptsInDirectory dir =  do         
        sortListDir <- fmap L.sort $ listDirectory dir
        return (map (\x -> dir ++ "/" ++ x) sortListDir)
    

    
    