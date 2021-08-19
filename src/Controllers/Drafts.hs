{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts
  ( routes
  ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecode, encode)
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Char8 as BC

import FromRequest
import Models.Draft
import Servises.Db
import Servises.Logger
import Servises.Token

routes ::
     Pool Connection
  -> Servises.Logger.Handle
  -> Servises.Token.Handle
  -> Servises.Db.Handle
  -> Request
  -> (Response -> IO b)
  -> IO b
routes pool hLogger hToken hDb req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing -> do
      logError hLogger "  Invalid or outdated token"
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just (id_author, _) -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post id_author
        "GET" -> get
        "PUT" -> put id_author
        "DELETE" -> delete id_author
        _ -> do
          logError hLogger "  Invalid method"
          respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
    -- draft creation (see example)
  where
    post id_author = do
      res <-
        strictRequestBody req >>= getDraft >>= postDraft id_author >>= postTags >>=
        postOtherPhotos
      case res of
        Nothing ->
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Just draft ->
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $
             encode draft)
      where
        getDraft body = do
          case eitherDecode body :: Either String DraftIn of
            Right draft -> return (Just draft)
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)
              return Nothing
        postDraft _ Nothing = return (0, Nothing)
        postDraft id_author_ (Just draft) = do
          c_d <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
          id_ <- insertDraft hDb pool draft id_author_ c_d
          case id_ of
            0 -> do
              logError hLogger "  Category not found"
              return (0, Nothing)
            _ -> return (id_, Just draft)
        postTags (id_, Just draft) = do
          let listTags = tags draft
          t <- mapM (insertTagDraft hDb pool id_) listTags
          when (t /= listTags) $ logWarning hLogger "  Not all tags were found"
          return (id_, Just (draft {tags = filter (/= 0) t}))
        postTags _ = return (0, Nothing)
        postOtherPhotos (id_, Just draft) = do
          let listPhotos = otherPhotos draft
          p <- mapM (insertPhotoDraft hDb pool id_) listPhotos
          when (p /= listPhotos) $
            logWarning hLogger "  Not all photos were found"
          return (Just (DraftPost id_ (tags draft) (filter (/= 0) p)))
        postOtherPhotos _ = return Nothing
    -- draft editing (see example)
    put id_author = do
      res <-
        strictRequestBody req >>= getDraft >>= putDraft id_author >>= putTags >>=
        putOtherPhotos
      case res of
        Nothing ->
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Just draft ->
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $
             encode draft)
      where
        getDraft body = do
          case eitherDecode body :: Either String DraftUp of
            Right draft -> return (Just draft)
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)
              return Nothing
        putDraft _ Nothing = return Nothing
        putDraft id_author' (Just draft) = do
          res <- updateDraft hDb pool draft id_author'
          case res of
            Just draft_ -> return (Just draft_)
            Nothing -> do
              logError hLogger "  Draft, category or main photo not found"
              return Nothing
        putTags Nothing = return Nothing
        putTags (Just draft)
          | isNothing (newTags draft) = return (Just draft)
          | otherwise = do
            deleteByID hDb pool "tag_draft" (id_draft draft)
            let listTags = fromMaybe [] (newTags draft)
            logDebug hLogger ("  " ++ show listTags)
            t <- mapM (insertTagDraft hDb pool (id_draft draft)) listTags
            when (t /= listTags) $
              logWarning hLogger "  Not all tags were found"
            return (Just (draft {newTags = Just (filter (/= 0) t)}))
        putOtherPhotos Nothing = return Nothing
        putOtherPhotos (Just draft)
          | isNothing (newOtherPhotos draft) = return (Just draft)
          | otherwise = do
            deleteByID hDb pool "photo_draft" (id_draft draft)
            let listPhotos = fromMaybe [] (newOtherPhotos draft)
            logDebug hLogger ("  " ++ show listPhotos)
            p <- mapM (insertPhotoDraft hDb pool (id_draft draft)) listPhotos
            when (p /= listPhotos) $
              logWarning hLogger "  Not all photos were found"
            return (Just (draft {newOtherPhotos = Just (filter (/= 0) p)}))
    -- deleting a drft
    delete id_author = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      deleteDraft hDb pool id_ id_author
      respond (responseLBS status204 [("Content-Type", "text/plain")] "")
    -- show draft, like
    -- http://localhost:3000/draft/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      draftMb <- liftIO $ findDraftByID hDb pool id_
      case draftMb of
        Nothing -> do
          logError hLogger "  Draft not exist"
          respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
        Just draft ->
          respond
            (responseLBS status200 [("Content-Type", "text/plain")] $
             encode draft)
