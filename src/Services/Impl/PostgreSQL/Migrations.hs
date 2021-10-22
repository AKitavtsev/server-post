module Services.Impl.PostgreSQL.Migrations
  ( runMigrations
  ) where

import Services.Logger

import Control.Monad (forM_, void)
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import System.Directory

import qualified Data.ByteString as BS
import qualified Data.List as L

runMigrations ::
     Services.Logger.Handle IO
  -> Connection
  -> Pool Connection
  -> FilePath
  -> IO ()
runMigrations hLogger conn pool dir = do
  begin conn
  fn <- scriptsInDirectory dir
  withResource pool $ \con -> do forM_ fn (executeMigration hLogger con)
  commit conn

executeMigration :: Services.Logger.Handle IO -> Connection -> FilePath -> IO ()
executeMigration hLogger con fileName = do
  content <- BS.readFile fileName
  void $ execute_ con (Query content)
  logInfo hLogger ("  Execute: " ++ fileName)
  return ()

-- | Lists all files in the given 'FilePath' 'dir' in alphabetical order.
scriptsInDirectory :: FilePath -> IO [FilePath]
scriptsInDirectory dir = do
  sortListDir <- L.sort <$> listDirectory dir
  return (map (\x -> dir ++ "/" ++ x) sortListDir)
