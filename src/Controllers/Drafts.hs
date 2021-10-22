{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts
  ( routes
  ) where

import Control.Monad (when)
import Data.Aeson (eitherDecode)
import Data.Maybe (fromMaybe, isNothing)

import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Char8 as BC

import FromRequest
import Models.Draft
import Services.Db
import Services.Logger
import Services.Token
import Utils

routes :: Monad m =>
     Services.Logger.Handle m
  -> Services.Token.Handle m
  -> Services.Db.Handle m
  -> FromRequest.HandleRequst m
  -> Request
  -> (Response -> m b)
  -> m b
routes hLogger hToken hDb hRequest req respond = do
  vt <- validToken hToken (toToken req)
  case vt of
    Nothing ->
      respondWithError hLogger respond status400 "  Invalid or outdated token"
    Just (id_author, _) -> do
      logInfo hLogger ("  Method = " ++ BC.unpack (toMethod req))
      case toMethod req of
        "POST" -> post id_author
        "GET" -> get
        "PUT" -> put id_author
        "DELETE" -> delete id_author
        _ -> respondWithError hLogger respond status404 "  Invalid method"
    -- draft creation (see example)
  where
    post id_author = do
      res <-
        toBody hRequest req >>= getDraft >>= postDraft >>= postTags >>=
        postOtherPhotos
      case res of
        Nothing -> respondWithError hLogger respond status400 ""
        Just draft -> respondWithSuccess respond status204 draft
      where
        getDraft body = do
          case eitherDecode body :: Either String RawDraft of
            Right draft -> return (Just draft)
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)
              return Nothing
        postDraft Nothing = return (0, Nothing)
        postDraft (Just draft) = do
          c_d <- curTimeStr hToken
          id_ <- insertDraft hDb draft id_author c_d
          case id_ of
            0 -> do
              logError hLogger "  Category not found"
              return (0, Nothing)
            _ -> return (id_, Just draft)
        postTags (id_, Just draft) = do
          let listTags = tags draft
          t <- mapM (insertTagDraft hDb id_) listTags
          when (t /= listTags) $ logWarning hLogger "  Not all tags were found"
          return (id_, Just (draft {tags = filter (/= 0) t}))
        postTags _ = return (0, Nothing)
        postOtherPhotos (id_, Just draft) = do
          let listPhotos = other_photos draft
          p <- mapM (insertPhotoDraft hDb id_) listPhotos
          when (p /= listPhotos) $
            logWarning hLogger "  Not all photos were found"
          return
            (Just
               (FoundTagsAndPhotosForDraft id_ (tags draft) (filter (/= 0) p)))
        postOtherPhotos _ = return Nothing
    -- draft editing (see example)
    put id_author = do
      res <-
        toBody hRequest req >>= getDraft >>= putDraft >>= putTags >>=
        putOtherPhotos
      case res of
        Nothing -> respondWithError hLogger respond status400 ""
        Just draft -> respondWithSuccess respond status200 draft
      where
        getDraft body = do
          case eitherDecode body :: Either String ForUpdateDraft of
            Right draft -> return (Just draft)
            Left e -> do
              logError hLogger ("  Invalid request body  - " ++ e)
              return Nothing
        putDraft Nothing = return Nothing
        putDraft (Just draft) = do
          res <- updateDraft hDb draft id_author
          case res of
            Just draft_ -> return (Just draft_)
            Nothing -> do
              logError hLogger "  Draft, category or main photo not found"
              return Nothing
        putTags Nothing = return Nothing
        putTags (Just draft)
          | isNothing (new_tags draft) = return (Just draft)
          | otherwise = do
            deleteByID hDb "tag_draft" (id_draft draft)
            let listTags = fromMaybe [] (new_tags draft)
            logDebug hLogger ("  " ++ show listTags)
            t <- mapM (insertTagDraft hDb (id_draft draft)) listTags
            when (t /= listTags) $
              logWarning hLogger "  Not all tags were found"
            return (Just (draft {new_tags = Just (filter (/= 0) t)}))
        putOtherPhotos Nothing = return Nothing
        putOtherPhotos (Just draft)
          | isNothing (new_other_photos draft) = return (Just draft)
          | otherwise = do
            deleteByID hDb "photo_draft" (id_draft draft)
            let listPhotos = fromMaybe [] (new_other_photos draft)
            logDebug hLogger ("  " ++ show listPhotos)
            p <- mapM (insertPhotoDraft hDb (id_draft draft)) listPhotos
            when (p /= listPhotos) $
              logWarning hLogger "  Not all photos were found"
            return (Just (draft {new_other_photos = Just (filter (/= 0) p)}))
    -- deleting a draft
    delete id_author = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      deleteDraft hDb id_ id_author
      respond (responseLBS status204 [("Content-Type", "text/plain")] "")
    -- show draft, like
    -- http://localhost:3000/draft/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get = do
      let id_ = toId req
      when (id_ == 0) $ do logError hLogger "  Invalid id_"
      draftMb <- findDraftByID hDb id_
      case draftMb of
        Nothing ->
          respondWithError hLogger respond status404 "  Draft not exist"
        Just draft -> respondWithSuccess respond status200 draft
