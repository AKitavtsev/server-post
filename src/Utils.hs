{-# LANGUAGE OverloadedStrings #-}

module Utils
  where

import Data.Aeson 
import Network.HTTP.Types
import Network.Wai

respondWith :: ToJSON a =>
     IO ()
  -> (Response -> IO b)
  -> Status
  -> a
  -> IO b  
respondWith logging respond status xs = do
  logging
  respond (responseLBS status [("Content-Type", "text/plain")] $ encode xs)
  

