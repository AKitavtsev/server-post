{-# LANGUAGE OverloadedStrings #-}


module FromRequest where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Time as Time

import Data.Char (isDigit)
import Network.Wai

toParam :: Request -> BC.ByteString -> Maybe String
toParam req name =
  case parBS of
    Nothing -> Nothing
    Just par -> Just (BC.unpack par)
  where
    parBS =
      foldl
        (\acc x ->
           if fst x == name
             then snd x
             else acc)
        Nothing $
      queryString req

toPath :: Request -> T.Text
toPath req =
  case pathInfo req of
    [] -> ""
    (x:_) -> x

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
    (_:_:z:_) -> read_ $ T.unpack z
    _ -> 0
  where
    read_ x =
      if x /= [] && all isDigit x
        then read x
        else 0

toIdImage :: Request -> Integer
toIdImage req =
  case pathInfo req of
    (_:y:_) -> read_ $ T.unpack y
    _ -> 0
  where
    read_ x =
      if (x /= []) && all isDigit x
        then read x
        else 0

curTimeStr :: IO String
curTimeStr =
  Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> Time.getCurrentTime

