{-# LANGUAGE OverloadedStrings #-}

module  ControllersTest.Data where

import Data.Functor.Identity (Identity)
import Network.HTTP.Types.Status
import Network.Wai

import qualified Services.Db as SD
import qualified Services.Logger as SL
import qualified Services.Token as ST

import FromRequest
import Models.Author
import Models.Category
import Models.Comment
import Models.Draft
import Models.Post
import Models.Tag
import Models.User

forShowUser :: ForShowUser
forShowUser = ForShowUser "Name" "Surname" "Avatar" "Login" "Date" False

rawUser :: RawUser
rawUser = RawUser "Name" "Surname" (Just (Avatar "image" "typ")) "Login" "Password"

authorsDetails :: AuthorsDetails
authorsDetails = AuthorsDetails "Name" "Surname" "Descr" "Avatar"

rawAuthor:: RawAuthor
rawAuthor = RawAuthor  1 "Description"

caregory :: Category 
caregory = Category "Name" (Just 1)

tag_ :: Tag
tag_ = Tag "tag" 

rawDraft  :: RawDraft
rawDraft = RawDraft  "title" 1 [1] "text_content" 1 [1]

photo :: Photo
photo = Photo "image" "typ"

forShowDraft :: ForShowDraft
forShowDraft = 
  ForShowDraft "title" "creation_date" 1 [1] "main_photo" ["other_photo"] "text_content"

forUpdateDraft :: ForUpdateDraft
forUpdateDraft = ForUpdateDraft
  1 (Just "title") (Just 1) (Just [1]) (Just "content") (Just 1) (Just [1])
  
rawComment :: RawComment
rawComment = RawComment 1 "comment"

post :: Post
post = Post 1 "title" "creation_date"
            authorsDetails
            (Categories "caregory" ["subсategories"])
            ["tag"] "photo" ["photo"] "content"

comment_ :: Comment
comment_ = Comment "creation_date" (User "name" "surname" "avatar") "comment"

reqPOST, reqHEAD, reqDELETE, reqPUT :: Request
reqPOST = defaultRequest {requestMethod = "POST"}
reqHEAD = defaultRequest {requestMethod = "HEAD"}
reqDELETE = defaultRequest {requestMethod = "DELETE"}
reqPUT = defaultRequest {requestMethod = "PUT"}

hTestTokenFalse = hTestToken {ST.validToken = \token -> return (Just (1, False))}
hTestTokenTrue = hTestToken {ST.validToken = \token -> return (Just (1, True))}

testRespond :: Response -> Identity Status
testRespond res = return (responseStatus res)

hTestLogger :: SL.Handle Identity
hTestLogger = SL.Handle {SL.logPriority = \prio msg -> return ()}

hTestToken :: ST.Handle Identity
hTestToken = ST.Handle
      { ST.createToken = \id_ adm -> return ""
      , ST.validToken = \token -> return Nothing
      , ST.curTimeStr = return ""
      }

hTestRequest :: HandleRequest Identity
hTestRequest = HandleRequest {toBody = \req -> return ""}


hTestDb :: SD.Handle Identity
hTestDb = SD.Handle
      { SD.limit = 1
      , SD.deleteByID = \model id_ -> return ()
      , SD.updateByID = \model id_ value -> return ()
      , SD.insertUser = \user creation_date -> return 0
      , SD.findUserByLogin = \login password -> return (Just (1, False))
      , SD.findUserByID = \ id_ -> return (Just forShowUser)
      , SD.insertImage = \rw id_ -> return 1
      , SD.findImageByID = \id_ -> return (Just("image", "type"))
      , SD.insertAuthor = \ra -> return 0
      , SD.findAuthorByID = \id_ -> return (Just authorsDetails)
      , SD.insertCategory = \cat -> return 0
      , SD.findCategoryByID = \id_ -> return (Just caregory)
      , SD.updateParentCategory = \id_ parent -> return 0
      , SD.insertTag = \t -> return ()
      , SD.insertTagDraft = \id_ t -> return 1
      , SD.insertPhotoDraft = \ id_ ph -> return 1
      , SD.findTags = \id_ auth -> return ["tag"] 
      , SD.insertDraft = \rd id_ date -> return 0
      , SD.deleteDraft = \id_ auth -> return ()
      , SD.updateDraft = \fud auth -> return (Just forUpdateDraft)
      , SD.insertPhoto = \ph -> return 0
      , SD.findTagByID = \id_ -> return (Just tag_)
      , SD.findPhotoByID = \id_ -> return (Just ("image", "typ"))
      , SD.findDraftByID = \id_ -> return (Just forShowDraft)
      , SD.publishPost = \draft auth -> return 1
      , SD.insertComment = \rc auth_id creation_date -> return 0
      , SD.deleteComment = \id_ auth -> return ()
      , SD.findAllPosts = \req limit -> return [post]
      , SD.findComments = \req limit id_post -> return [comment_]
      }