{-# LANGUAGE RankNTypes #-}

module Services.Token
  ( Handle(..)
  ) where

data Handle m =
  Handle
    { createToken :: Monad m => Integer -> Bool -> m String
    , validToken :: Monad m => String -> m (Maybe (Integer, Bool))
    , curTimeStr :: Monad m => m String
    }
