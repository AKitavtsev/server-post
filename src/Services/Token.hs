{-# LANGUAGE RankNTypes #-}

module Services.Token
  ( Handle(..)
  ) where

data Handle m =
  Handle
    { createToken :: Integer -> Bool -> m String
    , validToken :: String -> m (Maybe (Integer, Bool))
    , curTimeStr :: m String
    }
