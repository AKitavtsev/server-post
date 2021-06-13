{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


module Controllers.Drafts 
    where

import Control.Monad.Trans
import Data.Aeson (eitherDecode, encode )
import Data.Maybe (fromMaybe)
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
        "DELETE" -> delete id_author
        -- "PUT"    -> put id_author
  where
    post id_author = do    
      body <- strictRequestBody req
      logDebug hLogger ("  Body = " ++ (BL.unpack body))
      case eitherDecode body :: Either String DraftIn of
        Left e -> do
          logError hLogger ("  Invalid request body  - " ++ e)          
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Right correctlyParsedBody -> do
          case id_draft correctlyParsedBody of
            Nothing -> insert id_author correctlyParsedBody
            Just id_dr -> update id_dr id_author correctlyParsedBody            
    insert id_author correctlyParsedBody = do
      c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"          
      id     <- insertDraft hDb pool correctlyParsedBody id_author c_date
      case id of
        0 -> do
          logError hLogger "  No category specified" 
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        _ -> do
          idim   <- insertPhoto hDb pool id_author
                     (fromMaybe (Photo "" "jpg") (mainPhoto correctlyParsedBody))
          case idim of
            0 -> do
              logError hLogger
               "  Invalid photo type specified (only png, jpg, gif or bmp is allowed)"
              respond (responseLBS status400 [("Content-Type", "text/plain")] "")
            _ -> do
              updateByID hDb pool "draftPhoto" id (show idim)
              photos <- mapM (insertPhoto hDb pool id_author)
                             (fromMaybe [] (otherPhotos correctlyParsedBody))
              when (any (== 0) photos) $ do
                logWarning hLogger 
                       (" Some of the photos are of an invalid type" ++
                        " (only png, jpg, gif or bmp is allowed).")              
                updateByID hDb pool "draftPhotos" id (show photos)              
              respond (responseLBS status201 [("Content-Type", "text/plain")]
                       $ encode (DraftPost id idim (photos)))
    update id_dr id_author correctlyParsedBody = do
      when (not (title correctlyParsedBody == Nothing)) $ do
        id <- updateDraft hDb pool id_dr id_author
                    "title"
                    (fromMaybe "" (title correctlyParsedBody)) 
        return ()                    
      when (not (category correctlyParsedBody == Nothing)) $ do
        id <- updateDraft hDb pool  id_dr id_author
                    "category" 
                    $ show (fromMaybe 0 (category correctlyParsedBody))
        when (id == 0)  $ logWarning hLogger 
                       " No category specified"
        return ()
      when (not (tags correctlyParsedBody == Nothing)) $ do
        let tags' = fromMaybe [] (tags correctlyParsedBody)
        tags'' <- checkAvailabilityTags hDb pool tags'
        when (not (tags' == tags'')) $ logWarning hLogger 
          ("  Not all tags were found. Required - " ++ (show tags') 
                              ++ " , found - " ++ (show tags''))
        -- id <- updateDraft hDb pool id_dr id_author "tags" (show tags'')

        return ()

             
      respond (responseLBS status200 [("Content-Type", "text/plain")] "")
                              
    delete id_author = do
       let id   = toId req
       when (id == 0) $ do
         logError hLogger "  Invalid id"
       deleteDraft hDb pool id id_author
       respond (responseLBS status204 [("Content-Type", "text/plain")] "")
