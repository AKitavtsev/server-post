{-# LANGUAGE OverloadedStrings #-}

module FromRequest where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.Text as T

import Data.Maybe (fromMaybe)
import Network.Wai
import Text.Read (readMaybe)

newtype (Monad m) => HandleRequst m =
  HandleRequst
    {toBody :: Request -> m BL.ByteString}
    
hRequstIO :: HandleRequst IO
hRequstIO = HandleRequst {toBody = strictRequestBody}

toParam :: Request -> BC.ByteString -> Maybe String
toParam req name = fmap BC.unpack parBS
  where
    parBS =
      foldl
        (\acc x ->
           if fst x == name
             then snd x
             else acc)
        Nothing $
      queryString req

toPath :: Request -> String
toPath req =
  case pathInfo req of
    [] -> ""
    (x:_) -> T.unpack x

toMethod :: Request -> BC.ByteString
toMethod = requestMethod

toToken :: Request -> String
toToken req =
  case pathInfo req of
    (_:y:_) -> T.unpack y
    _ -> []

toId :: Request -> Integer
toId req =
  case pathInfo req of
    (_:_:z:_) -> fromMaybe 0 (readMaybe $ T.unpack z)
    _ -> 0

toIdImage :: Request -> Integer
toIdImage req =
  case pathInfo req of
    (_:y:_) -> fromMaybe 0 (readMaybe $ T.unpack y)
    _ -> 0


