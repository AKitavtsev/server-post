{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Drafts 
    where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
import Data.Pool (Pool)

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
-- import qualified Data.Time as Time

import Control.Monad (when)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

-- import Controllers.Token (Token (..))
import FromRequest
import Models.Draft
import Servises.Logger
import Servises.Token
import Servises.Db

routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (id_author, _) -> do
      logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))         
      case  toMethod req of
        "POST"   -> post id_author        
        -- "GET"    -> get
        -- "DELETE" -> delete
        -- "PUT"    -> put
  where 
    post id_author = do
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ (BL.unpack body))
      case eitherDecode body :: Either String DraftIn of
        Left e -> do
          logError hLogger ("  Invalid request body  - " ++ e)          
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Right correctlyParsedBody -> do
          c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"          
          id <- insertDraft hDb pool correctlyParsedBody id_author c_date
 
          idim <- insertPhoto hDb pool id_author (mainPhoto correctlyParsedBody)
          case idim of
            0 -> do
              logWarning hLogger "  Invalid image type specified (only png, jpg, gif or bmp is allowed)"
              respond (responseLBS status400 [("Content-Type", "text/plain")] "")
            _ -> do
              updateByID hDb pool "draftPhoto" id (show idim)
              photos <- mapM (insertPhoto hDb pool id_author) (otherPhotos correctlyParsedBody) 
              updateByID hDb pool "draftPhotos" id (show photos)
-- проверить photos на предмет нулей
              
              respond (responseLBS status201 [("Content-Type", "text/plain")]
                       $ encode (DraftPost id idim photos))
              
              
              
                                         

  -- where 
    -- post = do
      -- body <- strictRequestBody req
      -- logDebug hLogger ("  Body = " ++ (BL.unpack body))
      -- case eitherDecode body :: Either String UserIn of
        -- Left e -> do
          -- logError hLogger "  Invalid request body"         
          -- respond (responseLBS status400 [("Content-Type", "text/plain")] $ BL.pack e)
        -- Right correctlyParsedBody -> do


          -- case id of
            -- 0 -> do
              -- logError hLogger "  There is no user with this ID, or the user is already the author"
              -- respond (responseLBS status500 [("Content-Type", "text/plain")] "")
            -- _ -> respond (responseLBS created201 [("Content-Type", "text/plain")] "")    
    -- get = do
      -- let id   = toId req
      -- when (id == 0) $ do
        -- logError hLogger "  Invalid id"
      -- authorMb <- liftIO $ findAuthorByID hDb pool id
      -- case authorMb of
        -- Nothing -> do
          -- logError hLogger "  Author not exist"
          -- respond (responseLBS notFound404 [("Content-Type", "text/plain")] "author not exist")
        -- Just author -> do 
          -- respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode author)
    -- delete = do
       -- let id   = toId req
       -- when (id == 0) $ do
         -- logError hLogger "  Invalid id"
       -- deleteAuthorByID hDb pool id
       -- deleteByID hDb pool "author" id
       -- respond (responseLBS status204 [("Content-Type", "text/plain")] "")
    -- put = do
       -- let id      = toId req
           -- descrMb = toParam req "description"
       -- when (id == 0) $ do
         -- logError hLogger "  Invalid id"
       -- case descrMb of
         -- Nothing -> do
           -- logError hLogger "  The \"description\" parameter is required"
           -- respond (responseLBS status400 [("Content-Type", "text/plain")] "")
         -- Just descr -> do
           -- updateByID hDb pool "author" id descr
           -- respond (responseLBS status200 [("Content-Type", "text/plain")]
                   -- $ encode (Author id $ T.pack descr))

-- curTimeStr :: String -> IO String
-- curTimeStr form = do
    -- utc <- Time.getCurrentTime
    -- return (Time.formatTime Time.defaultTimeLocale form utc)         