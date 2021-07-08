{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Tags (routes)
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

-- import Controllers.Token (Token (..))
import FromRequest
import Models.Tag
-- import Servises.Config
import Servises.Logger
import Servises.Token
import Servises.Db

routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    _       -> do
      logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))         
      case  toMethod req of
        "POST"   -> post vt
        "GET"    -> get 
        "DELETE" -> delete vt
        "PUT"    -> put vt
        _        -> do 
          logError hLogger "  Invalid method"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""          
  where 
    post vt = do
      case vt of
        Just (_, True) -> do         
          body <- strictRequestBody req
          logDebug hLogger ("  Body = " ++ (BL.unpack body))
          case eitherDecode body :: Either String Tag of
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)          
              respond (responseLBS status400 [("Content-Type", "text/plain")] "")
            Right correctlyParsedBody -> do
              insertTag hDb pool correctlyParsedBody              
              respond (responseLBS created201 [("Content-Type", "text/plain")] "") 
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")    
    get = do
        let id  = toId req
        when (id == 0) $ do
          logError hLogger "  Invalid id"
        tagMb <- liftIO $ findTagByID hDb pool id
        case tagMb of
          Nothing -> do
            logError hLogger "  Tag not exist"
            respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
          Just tag -> do 
            respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode tag)
    delete vt = do
      case vt of
        Just (_, True) -> do
          let id  = toId req
          when (id == 0) $ do
            logError hLogger "  Invalid id"
          deleteByID hDb pool "tag" id
          respond (responseLBS status204 [("Content-Type", "text/plain")] "delete")
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")    
    put vt = do
      case vt of
        Just (_, True) -> do
          let id  = toId req
          when (id == 0) $ do
            logError hLogger "  Invalid id"
          let tagMb = (toParam req "tag")
          when (not (tagMb == Nothing)) $ do
            let tag = case tagMb of Just t -> t
            logError hLogger ("  Update tag to " ++ tag)
            updateByID hDb pool "tags" id "tag" tag
          respond (responseLBS status200 [("Content-Type", "text/plain")] "") 
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")    

