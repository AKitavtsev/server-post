{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import FromRequest
import Servises.Db
import Servises.Logger

import Control.Monad (when)
import Control.Monad.Trans
import Data.Pool (Pool)
import Database.PostgreSQL.Simple.Internal

import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)

-- show avatar, like
-- http://localhost:3000/image/1
routes :: Pool Connection
                -> Servises.Logger.Handle
                -> Servises.Db.Handle
                -> Request
                -> (Response -> IO b)
                -> IO b

routes pool hLogger hDb req respond = do
    let id_ = toIdImage req
    when (id_ == 0) $ logError hLogger "  Invalid id_"
    imageMb <- liftIO $ findImageByID hDb pool id_
    case imageMb of
        Nothing -> do
            logError hLogger "  Image not found"
            respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
        Just (image, typ) ->
            respond
                ( responseLBS
                    status200
                    [("Content-Type", BC.pack ("image/" ++ typ))]
                    $ BL.pack $ BC.unpack $ B64.decodeLenient $ BC.pack image
                )
