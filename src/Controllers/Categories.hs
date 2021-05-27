{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Categories (routes)
    where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
import Data.Pool (Pool)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Time as Time

import Control.Monad (when)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Database.PostgreSQL.Simple (Connection (..))

import Controllers.Token (Token (..))
import FromRequest
import Models.Author
-- import Servises.Config
import Servises.Logger
import Servises.Token
import Servises.Db

routes pool hLogger hToken hDb req respond = do
  let token = toToken req
  vt <- validToken hToken token
  when (vt == Nothing) $ do
         logError hLogger "  Invalid or outdated token"
  -- respond (responseLBS status400 [("Content-Type", "text/plain")] "")
  logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))         
  case  toMethod req of
        -- "POST" -> do
           -- body <- strictRequestBody req
           -- logDebug hLogger ("  Body = " ++ (BL.unpack body))
           -- case eitherDecode body :: Either String  Author of
             -- Left e -> do
               -- logError hLogger ("  Invalid request body  - " ++ e)          
               -- respond (responseLBS status400 [("Content-Type", "text/plain")] "")
             -- Right correctlyParsedBody -> do
-- здесь бы проверить наличие id в users (а может и нет?)
               -- insertAuthor hDb pool correctlyParsedBody              
               -- respond (responseLBS created201 [("Content-Type", "text/plain")] "")    
    "GET"    -> do
      let id   = toId req
      when (id == 0) $ do
        logError hLogger "  Invalid id"
      cat <- getCategories  hDb pool id
      logInfo hLogger (show cat)
      respond (responseLBS status200 [("Content-Type", "text/plain")] "")
          -- authorMb <- liftIO $ findAuthorByID hDb pool id
          -- case authorMb of
            -- Nothing -> do
              -- logError hLogger "  Author not exist"
              -- respond (responseLBS notFound404 [("Content-Type", "text/plain")] "author not exist")
            -- Just author -> do 
              -- respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode author)
              
        -- getCategories hDb pool id
        
        
        -- "DELETE" -> do
          -- let id   = toId req
          -- when (id == 0) $ do
            -- logError hLogger "  Invalid id"
          -- deleteAuthorByID hDb pool id
          -- respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
        -- "PUT"    -> do
          -- let id    = toId req
              -- descrMb = toParam req "description"
          -- when (id == 0) $ do
            -- logError hLogger "  Invalid id"
          -- case descrMb of
            -- Nothing -> do
              -- logError hLogger "  The \"description\" parameter is required"
              -- respond (responseLBS status400 [("Content-Type", "text/plain")] "")
            -- Just descr -> do
              -- updateAuthor hDb pool id descr
              -- respond (responseLBS status200 [("Content-Type", "text/plain")]
                        -- $ encode (Author id $ Just (T.pack descr)))
                
getCategories :: Servises.Db.Handle ->  Pool Connection -> Integer -> IO [Integer]
getCategories  hDb pool id = 
  helper hDb pool id []
      
    where
      helper :: Servises.Db.Handle -> Pool Connection -> Integer -> [Integer] -> IO [Integer]   
      helper hDb pool id listCat = do
        -- return []
    
            cats <- findCategory hDb pool id
            case cats of 
              Just (id, 0)    -> return (id:listCat )              
              Just (id, idOw) -> do
                helper hDb pool idOw (id:listCat)
              Nothing         -> return listCat

  
          