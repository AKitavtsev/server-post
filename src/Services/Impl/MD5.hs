module Services.Impl.MD5
  ( newHandle
  ) where

import qualified Config as SC
import qualified Services.Token as ST

import Data.Hash.MD5
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Text.Read (readMaybe)
import Data.Time

import Services.Impl.MD5.Internal


newHandle :: SC.Config -> IO (ST.Handle IO)
newHandle config = do
  return $
    ST.Handle
      { ST.createToken = createToken
      , ST.validToken = validToken
      , ST.curTimeStr = curTimeStr      
      }
  where
    createToken id_ adm = do
      time <- expirationTime
      let admStr =
            if adm
              then "1"
              else "0"
          idAdmTime = show id_ ++ "." ++ admStr ++ time
      return (idAdmTime ++ md5s (Str idAdmTime))
    validToken token = do
      timeUtc <- getCurrentTime
      case idAdmFromToken token of
        Nothing -> return Nothing
        Just (id_, adm) -> do
          let tt = timeFromToken token
              content = id_ ++ "." ++ adm ++ tt
              sample = content ++ md5s (Str content)
              res =
                (token == sample) &&
                (tt > timeStr timeUtc)          
          (if res then
             (do let admin = adm == "1"
                 return  (Just (fromMaybe 0 $ readMaybe id_ :: Integer, admin)))
           else
                 return Nothing)
    expirationTime = do
      ct <- getCurrentTime
      let lt = SC.lifetime config
      let et = addUTCTime (fromInteger lt :: NominalDiffTime) ct
      return (timeStr et)
    timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    curTimeStr = timeStr <$> getCurrentTime