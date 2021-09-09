module Services.Token
  ( Handle(..)
  ) where

data Handle =
  Handle
    { createToken :: Integer -> Bool -> IO String
    , validToken :: String -> IO (Maybe (Integer, Bool))
    }
