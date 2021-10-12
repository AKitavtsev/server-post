module Services.Impl.MD5
  ( newHandle
  ) where

import qualified Config as SC
import qualified Services.Token as ST

import Data.Hash.MD5
import Data.Time.Clock
import Services.Impl.MD5.Internal

import qualified Data.Time as Time

newHandle :: SC.Config -> IO ST.Handle
newHandle config = do
  return $
    ST.Handle
      { ST.createToken = createToken
      , ST.validToken = validToken
      }
  where
    createToken id_ adm = do
      time <- expirationTime config
      let admStr =
            if adm
              then "1"
              else "0"
          idAdmTime = show id_ ++ "." ++ admStr ++ time
      return (idAdmTime ++ md5s (Str idAdmTime))
    validToken token = do
      utc <- Time.getCurrentTime
      case idAdmFromToken token of
        Nothing -> return Nothing
        Just (id_, adm) -> do
          let tt = timeFromToken token
              content = id_ ++ "." ++ adm ++ tt
              sample = content ++ md5s (Str content)
              res =
                (token == sample) &&
                (tt > Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" utc)          
          (if res then
             (do let admin = adm == "1"
                 return (Just (read id_ :: Integer, admin)))
           else
                 return Nothing)

expirationTime :: SC.Config -> IO String
expirationTime config = do
  ct <- Time.getCurrentTime
  let lt = SC.lifetime config
  let et = addUTCTime (fromInteger lt :: NominalDiffTime) ct
  return (Time.formatTime Time.defaultTimeLocale "%Y%m%d%H%M%S" et)
