{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  Main where

import TokenTest
import DbTest
import FromRequestTest

main :: IO ()
main = do
    tokenTest
    dbTest
    fromRequestTest
