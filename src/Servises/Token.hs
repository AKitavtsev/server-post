module Servises.Token (Handle(..)) 
    where

import Servises.Config (Config (..))

data Handle = Handle
    { config      :: Config
    , createToken :: Integer -> Bool -> IO String
    , validToken  :: String -> IO (Maybe (Integer, Bool))
    }

