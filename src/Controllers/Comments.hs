{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Controllers.Comments 
    where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Control.Monad (when)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import FromRequest
import Models.Comment
import Servises.Logger
import Servises.Token
import Servises.Db

routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (id_author, adm) -> do
      logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))         
      case  toMethod req of
        "POST"   -> post id_author        
        -- "GET"    -> get
        "DELETE" -> delete id_author adm
        _        -> do 
          logError hLogger "  Invalid method"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
          


  where
    post id_author = do    
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ (BL.unpack body))
      case eitherDecode body :: Either String CommentIn of
        Right correctlyParsedBody -> do 
          c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
          id     <- insertComment hDb pool correctlyParsedBody id_author c_date
          case id of
            0 -> do
              logError hLogger "  post not found" 
              respond (responseLBS status400 [("Content-Type", "text/plain")] "")
            _ -> respond (responseLBS status201 [("Content-Type", "text/plain")]
                          $ encode (CommentID id))         
        Left  e -> do              
              logError hLogger ("  Invalid request body  - " ++ e)          
              respond (responseLBS status400 [("Content-Type", "text/plain")] "")
                                            
    delete id_author adm = do
       let id   = toId req
       case id of
         0 -> do
           logError hLogger "  Invalid id"
           respond (responseLBS status400 [("Content-Type", "text/plain")] "")
         _ -> do
           case adm of
             True  -> deleteByID hDb pool "comment" id
             False -> deleteComment hDb pool id id_author
           respond (responseLBS status204 [("Content-Type", "text/plain")] "")
       