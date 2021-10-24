
module  Main where

import TokenTest
import DbTest
import FromRequestTest
import ControllersTest.AuthorsTest
import ControllersTest.UsersTest

main :: IO ()
main = do
  tokenTest
  dbTest
  fromRequestTest
  authorsTest
  usersTest