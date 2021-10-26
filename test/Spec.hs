
module  Main where

import TokenTest
import DbTest
import FromRequestTest
import ControllersTest.AuthorsTest
import ControllersTest.CategoriesTest
import ControllersTest.CommentsTest
import ControllersTest.DraftsTest
import ControllersTest.ImagesTest
import ControllersTest.PhotosTest
import ControllersTest.PostsTest
import ControllersTest.PublishTest
import ControllersTest.TagsTest
import ControllersTest.TokenTest
import ControllersTest.UsersTest

main :: IO ()
main = do
  tokenMD5Test
  dbTest
  fromRequestTest
  authorsTest
  categoriesTest
  commentsTest
  draftsTest
  imagesTest
  photosTest
  postsTest
  publishTest
  tagsTest
  tokenTest
  usersTest
