{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  DbTest where

import Network.Wai
import Test.Hspec

import Servises.Impl.PostgreSQL.Internal
import Models.Draft (DraftUp (..))

req = defaultRequest 
       { requestMethod = "GET"
       , queryString = [ ("tags_in",Just "[1,2]")
                       , ("title",Just "Paul M")
                       , ("created_gt",Just "2021-07-10")
                       , ("name",Just "Bred")
                       , ("category",Just "5")
                       , ("find",Just "Pau")
                       , ("text",Just "McC")]
       , pathInfo =    ["posts"
                        ,"1.120210901202553ff034f3847c1d22f091dde7cde045264"
                        , "1"]
       }
draft = DraftUp { id_draft = 1
                , newTitle = Just "it is Title"
                , newCategory = Just 5
                , newTags = Just [1, 3, 10]
                , newContent = Just "it is content"
                , newMainPhoto = Just 4
                , newOtherPhotos = Just [1, 3, 10]
                }                    

dbTest :: IO ()
dbTest = hspec $ do
    describe "Servises.Impl.PostgreSQL.Internal" $ do
      describe "fromPhotoId" $ do
        it "link to photo" $ 
          fromPhotoId 1 `shouldBe` "http://localhost:3000/photo/1"
      describe "toListString" $ do
        it "list of String" $ 
          toListString "{aaa, bb, c}" `shouldBe` ["aaa", "bb", "c"]
      describe "toListInteger" $ do
        it "list of Integer" $ 
          toListInteger "{1, 2, 3}" `shouldBe` [1, 2, 3]
      describe "read'" $ do
        it "is integer" $ 
          read' "123" `shouldBe` 123
        it "is wrong integer " $ 
          read' "12h" `shouldBe` 1
      describe "partQuery draft" $ do
        it "Updated all fields" $ 
           partQuery draft `shouldBe`
           " title = 'it is Title', category_id =5, t_content ='it is content', photo_id =4"
        it "Updeted only title " $ 
           partQuery (DraftUp 1 (Just "it is Title") 
                      Nothing Nothing Nothing Nothing Nothing)
             `shouldBe` " title = 'it is Title'"
        it "Updated title and photo" $ 
           partQuery (DraftUp 1 (Just "it is Title")
                      Nothing Nothing Nothing (Just 4) Nothing)
             `shouldBe` " title = 'it is Title', photo_id =4"
        it "nothing unupdated" $ 
           partQuery (DraftUp 1 Nothing
                      Nothing Nothing Nothing Nothing Nothing)
             `shouldBe` ""
      describe "queryWhereTag" $ do
        it "tag=2" $
          queryWhereTag (req {queryString =[ ("tag",Just "2")]})
            `shouldBe` 
             " array_position ( ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id),2) IS NOT NULL AND"
        it "tags_in=[1,2]" $
          queryWhereTag req `shouldBe` 
           "  ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) && ARRAY [1,2] AND"
        it "tags_all=[1,2]" $
          queryWhereTag (req {queryString =[("tags_all",Just "[1,2]")]})
            `shouldBe` "  ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) @> ARRAY [1,2] AND"
        it "no tags" $
          queryWhereTag (req {queryString =[("title",Just "Paul M")]})
            `shouldBe` ""
      describe "queryWhereTitle" $ do
        it "title" $
          queryWhereTitle req `shouldBe` 
             " title LIKE '%Paul M%' AND"
        it "no title" $
          queryWhereTitle (req {queryString =[]}) `shouldBe` ""
      describe "queryWhereText" $ do
        it "text content" $
          queryWhereText req `shouldBe` " t_content LIKE '%McC%' AND"
        it "no text content" $
          queryWhereText (req {queryString =[]}) `shouldBe` ""
      describe "queryWhereDate" $ do
        it "created_gt=2021-07-10" $
          queryWhereDate req `shouldBe`
           " draft_date :: date >'2021-07-10' AND"
        it "created_lt=2021-07-10" $
          queryWhereDate (req {queryString =[("created_lt",Just "2021-07-10")]})
           `shouldBe` " draft_date :: date <'2021-07-10' AND"
        it "created_at=2021-07-10" $
          queryWhereDate (req {queryString =[("created_at",Just "2021-07-10")]})
           `shouldBe` " draft_date :: date ='2021-07-10' AND"
        it "no date" $
          queryWhereDate (req {queryString =[]})
           `shouldBe` ""
      describe "queryWhereAuthor" $ do
        it "author" $
          queryWhereAuthor req `shouldBe`
           " user_name = 'Bred' AND"
        it "no author" $
          queryWhereAuthor (req {queryString =[]}) `shouldBe` ""
      describe "queryWhereCategory" $ do
        it "category" $
          queryWhereCategory req `shouldBe`
           " category_id = '5' AND"
        it "no category" $
          queryWhereCategory (req {queryString =[]}) `shouldBe` ""
      describe "queryWhereFind" $ do
        it "find" $
           queryWhereFind req `shouldBe`
           " (array_position ( ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id), 'Pau') IS NOT NULL OR t_content LIKE '%Pau%' OR title LIKE '%Pau%' OR user_name LIKE '%Pau%' OR category_name LIKE '%Pau%') AND"
        it "no find" $
           queryWhereFind (req {queryString =[]}) `shouldBe`
           ""
      describe "queryWhere" $ do
        it "full WHERE" $
           queryWhere req `shouldBe`
           " WHERE  ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) && ARRAY [1,2] AND title LIKE '%Paul M%' AND t_content LIKE '%McC%' AND draft_date :: date >'2021-07-10' AND user_name = 'Bred' AND category_id = '5' AND (array_position ( ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id), 'Pau') IS NOT NULL OR t_content LIKE '%Pau%' OR title LIKE '%Pau%' OR user_name LIKE '%Pau%' OR category_name LIKE '%Pau%')"
        it "no WHERE" $
           queryWhere (req {queryString = []}) `shouldBe`
           ""
      describe "queryOrder" $ do
        it "ORDER BY date" $
            queryOrder (req {queryString = [("order",Just "[date]")]})
            `shouldBe` " ORDER BY draft_date"
        it "ORDER BY author" $
            queryOrder (req {queryString = [("order",Just "[author]")]})
            `shouldBe` " ORDER BY user_name"
        it "ORDER BY category" $
            queryOrder (req {queryString = [("order",Just "[category]")]})
            `shouldBe` " ORDER BY category_name"
        it "ORDER BY photo" $
            queryOrder (req {queryString = [("order",Just "[photo]")]})
            `shouldBe`
            " ORDER BY (SELECT count (*) FROM getphotos WHERE d_id = draft_id)"
        it "full ORDER BY" $
            queryOrder (req {queryString = [
                       ("order",Just "[date, author, category, photo]")]})
            `shouldBe`
            " ORDER BY draft_date, user_name, category_name, (SELECT count (*) FROM getphotos WHERE d_id = draft_id)"
        it "no ORDER BY" $
            queryOrder (req {queryString = []}) `shouldBe` ""

        



