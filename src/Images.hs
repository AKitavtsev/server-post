-- еще нюанс есть с content-type. Чтобы браузер показал картинку правильно, надо вернуть заголовок с типом содержимого, к примеру, Content-Type: image/png . У меня в реквесте на создание юзера требовалось передавать content type, я его хранил в базе рядом с содержимым.еще нюанс есть с content-type. Чтобы браузер показал картинку правильно, надо вернуть заголовок с типом содержимого, к примеру, Content-Type: image/png . У меня в реквесте на создание юзера требовалось передавать content type, я его хранил в базе рядом с содержимым.

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Images
    where

import FromRequest
import Db
import Token


import Control.Monad.Trans
-- import Database.PostgreSQL.Simple
-- import Data.Pool (Pool)
-- import Network.HTTP.Types.Status


import Data.Aeson
-- import Data.Char (isDigit)
-- import Data.Hash.MD5
-- import Data.Pool (Pool)
-- import Data.Time.Clock
import Data.List (dropWhile)
import GHC.Generics
import Network.HTTP.Types.Status
import Network.HTTP.Types
import Network.Wai

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64


routes pool req respond = do

--Сначало вставляем
-- Вариант 1 - из тела запроса POST
  -- body <- strictRequestBody req
  -- case  eitherDecode body :: Either String Image of
      -- Left e -> do
        -- respond (responseLBS status400 [("Content-Type", "text/plain")] "bad")
      -- Right (Image image) -> do
        -- insertImage pool image
--Вариант 2 - из файла
  -- imageFile <- BC.readFile "Images/kita.jpg"
  -- let image = BC.unpack $ B64.encode imageFile
  -- insertImage' pool image

--Теперь отдаем
  let token = toToken req
  curtime <- liftIO $ curTimeStr "%Y%m%d%H%M%S"
  case  (testToken token curtime) of
    Left st -> do
       respond (responseLBS st [("Content-Type", "text/plain")] "")
    Right _ -> do
      let id = toId req
      imageMb <- liftIO $ findImageByID pool id
      case imageMb of
        Nothing -> do

-- чисто для служебного пользования, для проекта надо оставить только
          -- respond (responseLBS notFound404 
                   -- [("Content-Type", "text/plain")]
                    -- "image not exist")
          let file = toParam req "file"
          case file of
            Nothing   -> respond (responseLBS notFound404 
                                  [("Content-Type", "text/plain")]
                                  "image not exist")
            Just fn -> do
              imageFile <- BC.readFile ("Images/" ++ fn) 
              let image = BC.unpack $ B64.encode imageFile              
              insertImage' pool id image (tail $ dropWhile (\x -> not (x == '.')) fn)         
              respond (responseLBS notFound404 
                       [("Content-Type", "text/plain")]
                         "image adding image")    
--------------------
        Just (image, typ)  -> do
          let typeImage = BC.pack ("image/" ++ typ)    
          respond (responseLBS status200 [("Content-Type", typeImage)]
              $ BL.pack $ BC.unpack $ B64.decodeLenient $ BC.pack image)

  

