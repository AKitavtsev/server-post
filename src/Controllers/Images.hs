{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import FromRequest
import Servises.Config
import Servises.Db
import Servises.Logger
import Servises.Token

import Control.Monad (when)
import Control.Monad.Trans
import Data.Aeson
import Data.List (dropWhile)
import GHC.Generics
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.Wai

import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)

-- show avatar, like
-- http://localhost:3000/image/1
routes pool hLogger hToken hDb req respond = do
    let id = toIdImage req
    when (id == 0) $ logError hLogger "  Invalid id"
    imageMb <- liftIO $ findImageByID hDb pool id
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
