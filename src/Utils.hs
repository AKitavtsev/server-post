{-# LANGUAGE OverloadedStrings #-}

module Utils
  where

import Data.Aeson 
import Network.HTTP.Types
import Network.Wai

import Services.Logger

respondWithSuccess :: ToJSON a =>
     (Response -> IO b)
  -> Status
  -> a
  -> IO b  
respondWithSuccess respond status context =
  respond (responseLBS status [("Content-Type", "text/plain")] $ encode context)
  
respondWithError :: 
     Services.Logger.Handle   
  -> (Response -> IO b)
  -> Status
  -> String
  -> IO b  
respondWithError hLogger respond status message= do
  logError hLogger message
  respond (responseLBS status [("Content-Type", "text/plain")] "")
