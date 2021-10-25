
module  Main where

import TokenTest
import DbTest
import FromRequestTest
import ControllersTest.AuthorsTest
import ControllersTest.CategoriesTest
import ControllersTest.CommentsTest
import ControllersTest.DraftsTest
import ControllersTest.ImagesTest
import ControllersTest.UsersTest

main :: IO ()
main = do
  tokenTest
  dbTest
  fromRequestTest
  authorsTest
  categoriesTest
  commentsTest
  draftsTest
  imagesTest
  usersTest
