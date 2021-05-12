{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Token
    where

import FromRequest
import Db

-- import Control.Monad.Trans
-- import Database.PostgreSQL.Simple
-- import Data.Pool (Pool)
-- import Network.HTTP.Types.Status

import Control.Monad.Trans
import Data.Aeson
import Data.Char (isDigit)
import Data.Hash.MD5
import Data.Pool (Pool)
import Data.Time.Clock
import GHC.Generics
-- import Network.HTTP.Types.Status
import Network.HTTP.Types
import Network.Wai


-- import Web.Scotty

import qualified Data.Time as Time


data Token = Token {token :: String}  
             deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- routes :: Pool Connection -> Request
       -- -> (Response -> IO ResponseReceived)
       -- -> IO ResponseReceived
routes pool req respond = do
  case  toParam req "login" of
    Nothing -> do
      respond (responseLBS status400 [("Content-Type", "text/plain")] "")
    Just login -> do
      case toParam req "password" of
        Nothing -> do 
          respond (responseLBS status400 [("Content-Type", "text/plain")] "")
        Just password -> do
          idAdm   <- liftIO $ findUserByLogin pool login password 
          timeStr <- liftIO expirationTime    
          case creatToken idAdm timeStr of
            Nothing -> do
              respond (responseLBS notFound404 [("Content-Type", "text/plain")] "")
            Just token -> do
              respond (responseLBS status200 [("Content-Type", "text/plain")] $ encode token)

curTimeStr :: String -> IO String
curTimeStr form = do
    utc <- Time.getCurrentTime
    return (Time.formatTime Time.defaultTimeLocale form utc)
    
expirationTime :: IO String
expirationTime = do
    ct <- Time.getCurrentTime
    let et = addUTCTime  (86400 :: NominalDiffTime) ct
    return (Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" et)

creatToken :: Maybe (Integer, Bool) -> String -> Maybe Token
creatToken (Just (id, adm))  time = 
    Just (Token (idAdmTime  ++ (md5s $ Str idAdmTime)))
    where admStr = if adm then  "1" else "0"
          idAdmTime = show id ++ "." ++ admStr ++ time
creatToken Nothing _ =  Nothing

idAdmFromToken :: String -> Maybe (Integer, Bool)
idAdmFromToken tok = case (takeWhile isDigit tok) of
                      [] -> Nothing
                      xs -> Just (read xs :: Integer, adm)
    where adm = case (dropWhile isDigit tok) of
                      ('.':'1':_) -> True
                      _           -> False
                      
timeFromToken :: String -> String
timeFromToken tok = case (dropWhile isDigit tok) of                      
                      ('.':_:xs) -> take 14 xs
                      _ -> []

testToken :: String -> String -> Either Status (Integer, Bool)
testToken tok ct = case (creatToken  iat tt) of 
                     Nothing -> Left notFound404
                     Just (Token tok') ->
                       if (not (tok == tok')) then Left unauthorized401
                       else if (tt < ct) then Left $ mkStatus 401 "request new token"
                            else Right iat'
   where tt  = timeFromToken tok
         iat  = idAdmFromToken tok
         iat' = iatFromMb $ idAdmFromToken tok
         iatFromMb (Just i) = i                 


