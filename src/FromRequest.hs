{-# LANGUAGE OverloadedStrings #-}

module FromRequest
    where

-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
-- import Data.ByteString (pack)
 
import Network.Wai

    
toParam :: Request -> BS.ByteString -> Maybe String
toParam req name = case parBS of
                     Nothing -> Nothing
                     Just par -> Just (BS.unpack par)
    where 
      parBS = foldl (\acc x -> if fst x == name then snd x else acc)
                         Nothing $ queryString req

toPath :: Request -> T.Text
toPath req = head $ pathInfo req

toMethod :: Request -> BS.ByteString
toMethod req = requestMethod req

toToken :: Request -> String
toToken req = case pathInfo req of
                (x:y:xs) -> T.unpack y

toId :: Request -> Integer
toId req = case pathInfo req of
                (x:y:z:xs) -> read $ T.unpack z
