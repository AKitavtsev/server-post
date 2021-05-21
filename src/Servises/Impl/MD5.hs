module Servises.Impl.MD5
  ( newHandle
  ) where

import qualified Servises.Token as ST
import qualified Servises.Config as SC

import Control.Monad (when)
import Data.Char (isDigit)
import Data.Hash.MD5
import Data.Time.Clock


import qualified Data.Time as Time


-- import Control.Monad (when)

newHandle :: SC.Config -> IO ST.Handle
newHandle config = do
  return $ ST.Handle
    { ST.config = config
    , ST.createToken = createToken
    , ST.validToken  = validToken
    }      
    where
      createToken id adm = do
        time <- expirationTime config 
        let admStr    = if adm then  "1" else "0"
            idAdmTime = show id ++ "." ++ admStr ++ time
        return (idAdmTime  ++ (md5s $ Str idAdmTime))
      validToken token = do
        utc <- Time.getCurrentTime     
        case idAdmFromToken token of
          Nothing -> return Nothing
          Just (id, adm) -> do
            let tt      = timeFromToken token
                content = id ++ "."++ adm ++ tt        
                sample  = content ++ (md5s $ Str content)
                res     = (token == sample) && 
                      (tt > Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" utc)
            case res of 
              False -> return Nothing
              True -> do
                let admin = if adm == "1" then  True else False
                return (Just (read id :: Integer, admin))
     
expirationTime :: SC.Config -> IO String
expirationTime config = do
    ct <- Time.getCurrentTime
    let lt = case config of
               (SC.TokenConfig x) -> x
    let et = addUTCTime  (fromInteger lt :: NominalDiffTime) ct
    return (Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" et)
    
idAdmFromToken :: String -> Maybe (String, String)
idAdmFromToken tok = case (takeWhile isDigit tok) of
                      [] -> Nothing
                      xs -> Just (xs, adm)
    where adm = case (dropWhile isDigit tok) of
                      ('.':'1':_) -> "1"
                      _           -> "0"
                      
timeFromToken :: String -> String
timeFromToken tok = case (dropWhile isDigit tok) of                      
                      ('.':_:xs) -> take 14 xs
                      _ -> []
    
