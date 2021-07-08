{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Authors 
    where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
import Data.Pool (Pool)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import Control.Monad (when)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Author
import Servises.Logger
import Servises.Token
import Servises.Db

routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (id, False) -> do
      logError hLogger "  Administrator authority required"
      respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")    
    Just (id, True) -> do
      logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))         
      case  toMethod req of
        "POST"   -> post         
        "GET"    -> get
        "DELETE" -> delete
        "PUT"    -> put
        _        -> do 
          logError hLogger "  Invalid method"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""          
  where 
    post = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ (BL.unpack body))
      case eitherDecode body :: Either String  Author of
        Left e -> do
          logError hLogger ("  Invalid request body  - " ++ e)          
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Right correctlyParsedBody -> do
          id <- insertAuthor hDb pool correctlyParsedBody
          case id of
            0 -> do
              logError hLogger "  There is no user with this ID, or the user is already the author"
              respond (responseLBS status500 [("Content-Type", "text/plain")] "")
            _ -> respond (responseLBS created201 [("Content-Type", "text/plain")] "")    
    get = do
      let id   = toId req
      when (id == 0) $ do
        logError hLogger "  Invalid id"
      authorMb <- liftIO $ findAuthorByID hDb pool id
      case authorMb of
        Nothing -> do
          logError hLogger "  Author not exist"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "author not exist")
        Just author -> do 
          respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode author)
    delete = do
       let id   = toId req
       when (id == 0) $ do
         logError hLogger "  Invalid id"
       -- deleteAuthorByID hDb pool id
       deleteByID hDb pool "author" id
       respond (responseLBS status204 [("Content-Type", "text/plain")] "")
    put = do
       let id      = toId req
           descrMb = toParam req "description"
       when (id == 0) $ do
         logError hLogger "  Invalid id"
       case descrMb of
         Nothing -> do
           logError hLogger "  The \"description\" parameter is required"
           respond (responseLBS status400 [("Content-Type", "text/plain")] "")
         Just descr -> do
           updateByID hDb pool "authors" id "description" descr
           respond (responseLBS status200 [("Content-Type", "text/plain")]
                   $ encode (Author id $ T.pack descr))

          