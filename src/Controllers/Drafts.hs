{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecode, encode)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import FromRequest
import Models.Draft
import Servises.Db
import Servises.Logger
import Servises.Token

routes pool hLogger hToken hDb req respond = do
    vt <- validToken hToken (toToken req)
    case vt of
        Nothing -> do
            logError hLogger "  Invalid or outdated token"
            respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Just (id_author, _) -> do
            logInfo hLogger ("  Method = " ++ (BC.unpack $ toMethod req))
            case toMethod req of
                "POST" -> post id_author
                "GET" -> get id_author
                "PUT" -> put id_author
                "DELETE" -> delete id_author
                _ -> do
                    logError hLogger "  Invalid method"
                    respond $ responseLBS status404 [("Content-Type", "text/plain")] ""
  where
    -- draft creation (see example)
    post id_author = do
        res <-
            strictRequestBody req
                >>= getDraft
                >>= postDraft id_author
                >>= postTags
                >>= postOtherPhotos
        case res of
            Nothing ->
                respond
                    ( responseLBS
                        status400
                        [("Content-Type", "text/plain")]
                        ""
                    )
            Just draft ->
                respond
                    ( responseLBS
                        status200
                        [("Content-Type", "text/plain")]
                        $ encode draft
                    )
      where
        getDraft body = do
            case eitherDecode body :: Either String DraftIn of
                Right draft -> return (Just draft)
                Left e -> do
                    logError hLogger ("  Invalid request body  - " ++ e)
                    return Nothing
        postDraft _ Nothing = return (0, Nothing)
        postDraft id_author (Just draft) = do
            c_date <- liftIO $ curTimeStr "%Y-%m-%d %H:%M:%S"
            id <- insertDraft hDb pool draft id_author c_date
            case id of
                0 -> do
                    logError hLogger "  Category not found"
                    return (0, Nothing)
                _ -> return (id, (Just draft))
        postTags (0, _) = return (0, Nothing)
        postTags (id, (Just draft)) = do
            let listTags = tags draft
            t <- mapM (insertTagDraft hDb pool id) listTags
            when (t /= listTags) $ logWarning hLogger "  Not all tags were found"
            return (id, Just (draft{tags = (filter (/= 0) t)}))
        postOtherPhotos (0, _) = return Nothing
        postOtherPhotos (id, (Just draft)) = do
            let listPhotos = otherPhotos draft
            p <- mapM (insertPhotoDraft hDb pool id) listPhotos
            when (p /= listPhotos) $ logWarning hLogger "  Not all photos were found"
            return (Just (DraftPost id (tags draft) (filter (/= 0) p)))
    -- draft editing (see example)
    put id_author = do
        res <-
            strictRequestBody req
                >>= getDraft
                >>= putDraft id_author
                >>= putTags
                >>= putOtherPhotos
        case res of
            Nothing ->
                respond
                    ( responseLBS
                        status400
                        [("Content-Type", "text/plain")]
                        ""
                    )
            Just draft ->
                respond
                    ( responseLBS
                        status200
                        [("Content-Type", "text/plain")]
                        $ encode draft
                    )
      where
        getDraft body = do
            case eitherDecode body :: Either String DraftUp of
                Right draft -> return (Just draft)
                Left e -> do
                    logError hLogger ("  Invalid request body  - " ++ e)
                    return Nothing
        putDraft _ Nothing = return Nothing
        putDraft id_author (Just draft) = do
            res <- updateDraft hDb pool draft id_author
            case res of
                Just draft -> return (Just draft)
                Nothing -> do
                    logError hLogger "  Draft, category or main photo not found"
                    return Nothing
        putTags Nothing = return Nothing
        putTags (Just draft)
            | newTags draft == Nothing = return (Just draft)
            | otherwise = do
                deleteByID hDb pool "tag_draft" (id_draft draft)
                let listTags = fromMaybe [] (newTags draft)
                logDebug hLogger ("  " ++ (show listTags))
                t <- mapM (insertTagDraft hDb pool (id_draft draft)) listTags
                when (t /= listTags) $ logWarning hLogger "  Not all tags were found"
                return (Just (draft{newTags = Just (filter (/= 0) t)}))
        putOtherPhotos Nothing = return Nothing
        putOtherPhotos (Just draft)
            | newOtherPhotos draft == Nothing = return (Just draft)
            | otherwise = do
                deleteByID hDb pool "photo_draft" (id_draft draft)
                let listPhotos = fromMaybe [] (newOtherPhotos draft)
                logDebug hLogger ("  " ++ (show listPhotos))
                p <- mapM (insertPhotoDraft hDb pool (id_draft draft)) listPhotos
                when (p /= listPhotos) $ logWarning hLogger "  Not all photos were found"
                return (Just (draft{newOtherPhotos = Just (filter (/= 0) p)}))
    -- deleting a drft
    delete id_author = do
        let id = toId req
        when (id == 0) $ do
            logError hLogger "  Invalid id"
        deleteDraft hDb pool id id_author
        respond (responseLBS status204 [("Content-Type", "text/plain")] "")
    -- show draft, like
    -- http://localhost:3000/draft/1.120210901202553ff034f3847c1d22f091dde7cde045264/1
    get id_author = do
        let id = toId req
        when (id == 0) $ do
            logError hLogger "  Invalid id"
        draftMb <- liftIO $ findDraftByID hDb pool id
        case draftMb of
            Nothing -> do
                logError hLogger "  Draft not exist"
                respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
            Just draft ->
                respond
                    ( responseLBS
                        status200
                        [("Content-Type", "text/plain")]
                        $ encode draft
                    )
