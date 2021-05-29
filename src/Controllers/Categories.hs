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
import Models.Category
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
  where 
    post vt = do
      case vt of
        Just (_, True) -> do         
          body <- strictRequestBody req
          logDebug hLogger ("  Body = " ++ (BL.unpack body))
          case eitherDecode body :: Either String Category of
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)          
              respond (responseLBS status400 [("Content-Type", "text/plain")] "")
            Right correctlyParsedBody -> do
              insertCategory hDb pool correctlyParsedBody              
              respond (responseLBS created201 [("Content-Type", "text/plain")] "") 
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")    
    get = do
        let id  = toId req
        when (id == 0) $ do
          logError hLogger "  Invalid id"
        categoryMb <- liftIO $ findCategoryByID hDb pool id
        case categoryMb of
          Nothing -> do
            logError hLogger "  Category not exist"
            respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
          Just category -> do 
            respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode category)
    delete vt = do
      case vt of
        Just (_, True) -> do
          let id  = toId req
          when (id == 0) $ do
            logError hLogger "  Invalid id"
          deleteCategoryByID hDb pool id
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
          let nameMb = (toParam req "name")

          when (not (nameMb == Nothing)) $ do
            let name = case nameMb of Just n -> n
            logInfo hLogger ("  Update name to " ++ name)
            updateNameCategory hDb pool id name
          let ownerMb = (toParam req "id_owner")           
          when (not (ownerMb == Nothing)) $ do
            let owner = case ownerMb of Just o -> o            
            logInfo hLogger ("  Update id_owner to " ++ owner)
            updateOwnerCategory hDb pool id owner
          respond (responseLBS status200 [("Content-Type", "text/plain")] "") 
        Just (_, False) -> do
          logError hLogger "  Administrator authority required"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "no admin")    

