{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Servises.Impl.PostgreSQL.Migrations (runMigrations)

    where
import Servises.Logger

import Control.Monad (forM_, void, when)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (..))
import Data.Pool
import System.Directory

import qualified Data.ByteString as BS 
import qualified Data.List as L

  
runMigrations :: Servises.Logger.Handle -> Connection -> Pool Connection -> FilePath -> IO ()
runMigrations hLogger conn pool dir = do
    begin conn
    fn <- scriptsInDirectory dir 
    withResource pool $ \conn -> do
      forM_ fn (executeMigration hLogger conn ) 
    commit conn
          
executeMigration :: Servises.Logger.Handle -> Connection -> FilePath -> IO ()
executeMigration hLogger con fileName  = do
            content <- BS.readFile fileName
            void $ execute_ con (Query content)
            logInfo hLogger ("  Execute: " ++ fileName)
            return () 

-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: FilePath -> IO [FilePath]
scriptsInDirectory dir =  do         
        sortListDir <- fmap L.sort $ listDirectory dir
        return (map (\x -> dir ++ "/" ++ x) sortListDir)
    

    
    