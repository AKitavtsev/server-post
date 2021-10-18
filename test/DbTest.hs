{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  DbTest where

import Network.Wai
import Test.Hspec

import Services.Impl.PostgreSQL.Internal
import Models.Draft (ForUpdateDraft (..))

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
draft = ForUpdateDraft { id_draft = 1
                , new_title = Just "it is Title"
                , new_category = Just 5
                , new_tags = Just [1, 3, 10]
                , new_content = Just "it is content"
                , new_main_photo = Just 4
                , new_other_photos = Just [1, 3, 10]
                }                    

dbTest :: IO ()
dbTest = hspec $ do
    describe "Services.Impl.PostgreSQL.Internal" $ do
      describe "fromPhotoId" $ do
        it "link to photo" $ 
          fromPhotoId "http://localhost:3000" 1 `shouldBe` "http://localhost:3000/photo/1"
      describe "toListString" $ do
        it "list of String" $ 
          toListString "{aaa, bb, c}" `shouldBe` ["aaa", "bb", "c"]
      describe "toListInteger" $ do
        it "list of Integer" $ 
          toListInteger "{1, 2, 3}" `shouldBe` [1, 2, 3]
      describe "bodyUpdate draft" $ do
        it "Updated all fields" $ 
           bodyUpdate draft `shouldBe`
           (" title = ?, category_id = ?, t_content = ?, photo_id = ?", ["it is Title", "5", "it is content", "4"])
        it "Updeted only title " $ 
           bodyUpdate (ForUpdateDraft 1 (Just "it is Title") 
                      Nothing Nothing Nothing Nothing Nothing)
             `shouldBe` (" title = ?", ["it is Title"])
        it "Updated title and photo" $ 
           bodyUpdate (ForUpdateDraft 1 (Just "it is Title")
                      Nothing Nothing Nothing (Just 4) Nothing)
             `shouldBe` (" title = ?, photo_id = ?", ["it is Title", "4"])
        it "nothing unupdated" $ 
           bodyUpdate (ForUpdateDraft 1 Nothing
                      Nothing Nothing Nothing Nothing Nothing)
             `shouldBe` ("", [])
      describe "queryWhereTag" $ do
        it "tag=2" $
          queryWhereTag (req {queryString =[ ("tag",Just "2")]}) `shouldBe` 
           (" array_position ( ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id), ?) IS NOT NULL AND", ["2"])
        it "tags_in=[1,2]" $
          queryWhereTag req `shouldBe` 
           (" ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) &&  ARRAY [1,2] AND",[])
        it "tags_all=[1,2]" $
          queryWhereTag (req {queryString =[("tags_all",Just "[1,2]")]}) `shouldBe`
           (" ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) @> ARRAY [1,2] AND",[])
        it "no tags" $
          queryWhereTag (req {queryString =[("title",Just "Paul M")]})
            `shouldBe` ("", [])
      describe "queryWhereTitle" $ do
        it "title" $
          queryWhereTitle req `shouldBe` 
             (" title LIKE ? AND", ["Paul M"])
        it "no title" $
          queryWhereTitle (req {queryString =[]}) `shouldBe` ("", [])
      describe "queryWhereText" $ do
        it "text content" $
          queryWhereText req `shouldBe` (" t_content LIKE ? AND", ["McC"])
        it "no text content" $
          queryWhereText (req {queryString =[]}) `shouldBe` ("", [])
      describe "queryWhereDate" $ do
        it "created_gt=2021-07-10" $
          queryWhereDate req `shouldBe`
           (" draft_date :: date >? AND", ["2021-07-10"])
        it "created_lt=2021-07-10" $
          queryWhereDate (req {queryString =[("created_lt",Just "2021-07-10")]})
           `shouldBe` (" draft_date :: date <? AND", ["2021-07-10"])
        it "created_at=2021-07-10" $
          queryWhereDate (req {queryString =[("created_at",Just "2021-07-10")]})
           `shouldBe` (" draft_date :: date =? AND", ["2021-07-10"])
        it "no date" $
          queryWhereDate (req {queryString =[]})
           `shouldBe` ("", [])
      describe "queryWhereAuthor" $ do
        it "author" $
          queryWhereAuthor req `shouldBe`
           (" user_name = ? AND", ["Bred"])
        it "no author" $
          queryWhereAuthor (req {queryString =[]}) `shouldBe` ("", [])
      describe "queryWhereCategory" $ do
        it "category" $
          queryWhereCategory req `shouldBe`
           (" category_id = ? AND", ["5"])
        it "no category" $
          queryWhereCategory (req {queryString =[]}) `shouldBe` ("", [])
      describe "queryWhereFind" $ do
        it "find" $
           queryWhereFind req `shouldBe`
           (
            " (array_position ( ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id), ?) IS NOT NULL OR t_content LIKE ? OR title LIKE ? OR user_name LIKE ? OR category_name LIKE ?) AND"
           , ["Pau","Pau","Pau","Pau","Pau"]
           )
        it "no find" $
           queryWhereFind (req {queryString =[]}) `shouldBe`
           ("", [])
      describe "queryWhere" $ do
        it "full WHERE" $
           queryWhere req `shouldBe`
           (
            " WHERE ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) &&  ARRAY [1,2] AND title LIKE ? AND t_content LIKE ? AND draft_date :: date >? AND user_name = ? AND category_id = ? AND (array_position ( ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id), ?) IS NOT NULL OR t_content LIKE ? OR title LIKE ? OR user_name LIKE ? OR category_name LIKE ?)"
            ,["Paul M","McC","2021-07-10","Bred","5","Pau","Pau","Pau","Pau","Pau"]
           )
        it "no WHERE" $
           queryWhere (req {queryString = []}) `shouldBe`
           ("", [])
      describe "queryOrder" $ do
        it "ORDER BY date" $
            queryOrder (req {queryString = [("order",Just "[date]")]})
            `shouldBe` (" ORDER BY draft_date", [])
        it "ORDER BY author" $
            queryOrder (req {queryString = [("order",Just "[author]")]})
            `shouldBe` (" ORDER BY user_name", [])
        it "ORDER BY category" $
            queryOrder (req {queryString = [("order",Just "[category]")]})
            `shouldBe` (" ORDER BY category_name", [])
        it "ORDER BY photo" $
            queryOrder (req {queryString = [("order",Just "[photo]")]})
            `shouldBe`
            (" ORDER BY (SELECT count (*) FROM getphotos WHERE d_id = draft_id)", [])
        it "full ORDER BY" $
            queryOrder (req {queryString = [
                       ("order",Just "[date, author, category, photo]")]})
            `shouldBe`
            (
              " ORDER BY draft_date, user_name, category_name, (SELECT count (*) FROM getphotos WHERE d_id = draft_id)"
            , [])
        it "no ORDER BY" $
            queryOrder (req {queryString = []}) `shouldBe` ("", [])

        



