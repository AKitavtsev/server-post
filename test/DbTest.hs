{-# LANGUAGE OverloadedStrings #-}

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
    describe "Testing Services.Impl.PostgreSQL.Internal" $ do
      describe "Getting a link to a photo by ID" $ do
        it "Should successfully get a link to a photo" $ 
          fromPhotoId "http://localhost:3000" 1 `shouldBe` "http://localhost:3000/photo/1"
      describe "Converting an SQL array to a list of String" $ do
        it "Should successfully convert" $ 
          toListString "{aaa, bb, c}" `shouldBe` ["aaa", "bb", "c"]
      describe "Converting an SQL array to a list of Integer" $ do
        it "Should successfully convert" $ 
          toListInteger "{1, 2, 3}" `shouldBe` [1, 2, 3]
        it "Should fail if syntax error" $ 
          toListInteger "{1, b, 3}" `shouldBe` []

      describe "Preparation of data for the formation of an SQL-query for some database operations" $ do
        describe "Forming data for a query \"UPDATE draft\"" $ do
          it "For updating all fields" $ 
            bodyUpdate draft `shouldBe`
            (" title = ?, category_id = ?, t_content = ?, photo_id = ?", ["it is Title", "5", "it is content", "4"])
          it "For updeting only title " $ 
            bodyUpdate (ForUpdateDraft 1 (Just "it is Title") 
                      Nothing Nothing Nothing Nothing Nothing)
             `shouldBe` (" title = ?", ["it is Title"])
          it "For updating title and photo" $ 
            bodyUpdate (ForUpdateDraft 1 (Just "it is Title")
                      Nothing Nothing Nothing (Just 4) Nothing)
             `shouldBe` (" title = ?, photo_id = ?", ["it is Title", "4"])
          it "No need to update anything" $ 
            bodyUpdate (ForUpdateDraft 1 Nothing
                      Nothing Nothing Nothing Nothing Nothing)
             `shouldBe` ("", [])
        describe "Forming data for a query \"SELECT FROM post\" with filtering" $ do
          describe "\"SELECT FROM post\" filtered by tag" $ do
            it "For a selection of posts by specific tag" $
              queryWhereTag (req {queryString =[ ("tag",Just "2")]}) `shouldBe` 
               (" array_position ( ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id), ?) IS NOT NULL AND", ["2"])
            it "Find articles that have at least one tag from the list" $
              queryWhereTag req `shouldBe` 
               (" ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) &&  ARRAY [1,2] AND",[])
            it "Find only those articles that have all the tags at the same time" $
              queryWhereTag (req {queryString =[("tags_all",Just "[1,2]")]}) `shouldBe`
              (" ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) @> ARRAY [1,2] AND",[])
            it "For a selection without filtering by tag" $
              queryWhereTag (req {queryString =[("title",Just "Paul M")]})
              `shouldBe` ("", [])
          describe "\"SELECT FROM post\" filtered by title" $ do
            it "For a selection of posts by the occurrence of a string into title" $
              queryWhereTitle req `shouldBe` 
             (" title LIKE ? AND", ["Paul M"])
            it "For a selection without filtering by title" $
              queryWhereTitle (req {queryString =[]}) `shouldBe` ("", [])
          describe "\"SELECT FROM post\" filtered by text" $ do
            it "For a selection of posts by the occurrence of a string into text" $
              queryWhereText req `shouldBe` (" t_content LIKE ? AND", ["McC"])
            it "For a selection without filtering by text" $
              queryWhereText (req {queryString =[]}) `shouldBe` ("", [])
          describe "\"SELECT FROM post\" filtered by date" $ do
            it "For a selection of posts created after the date" $
              queryWhereDate req `shouldBe`
              (" draft_date :: date >? AND", ["2021-07-10"])
            it "For a selection of posts created earlier the date" $
              queryWhereDate (req {queryString =[("created_lt",Just "2021-07-10")]})
              `shouldBe` (" draft_date :: date <? AND", ["2021-07-10"])
            it "For a selection of posts created in the same date" $
              queryWhereDate (req {queryString =[("created_at",Just "2021-07-10")]})
              `shouldBe` (" draft_date :: date =? AND", ["2021-07-10"])
            it "For a selection without filtering by date" $
              queryWhereDate (req {queryString =[]})
              `shouldBe` ("", [])
          describe "\"SELECT FROM post\" filtered by author" $ do
            it "For a selection of posts by the occurrence of a string into name of author" $
              queryWhereAuthor req `shouldBe`
              (" user_name = ? AND", ["Bred"])
            it "For a selection without filtering by author" $
              queryWhereAuthor (req {queryString =[]}) `shouldBe` ("", [])
          describe "\"SELECT FROM post\" filtered by category" $ do
            it "For a selection of posts by specific category" $
              queryWhereCategory req `shouldBe` (" category_id = ? AND", ["5"])
            it "For a selection without filtering by category" $
              queryWhereCategory (req {queryString =[]}) `shouldBe` ("", [])
          describe "to search posts by a string included either in the text, or in the name of the author, or in the name of the category/tag" $ do
            it "For a selection with filtering by string" $
              queryWhereFind req `shouldBe`
              (
              " (array_position ( ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id), ?) IS NOT NULL OR t_content LIKE ? OR title LIKE ? OR user_name LIKE ? OR category_name LIKE ?) AND"
              , ["Pau","Pau","Pau","Pau","Pau"]
              )
            it "For a selection without filtering by string" $
              queryWhereFind (req {queryString =[]}) `shouldBe`
              ("", [])
          describe "For a selection with filtering by several parameters" $ do
            it "For a selection with filtering by all imaginable fields" $
              queryWhere req `shouldBe`
              (
              " WHERE ARRAY (SELECT t_id FROM gettags WHERE d_id = draft_id) &&  ARRAY [1,2] AND title LIKE ? AND t_content LIKE ? AND draft_date :: date >? AND user_name = ? AND category_id = ? AND (array_position ( ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id), ?) IS NOT NULL OR t_content LIKE ? OR title LIKE ? OR user_name LIKE ? OR category_name LIKE ?)"
              ,["Paul M","McC","2021-07-10","Bred","5","Pau","Pau","Pau","Pau","Pau"]
              )
            it "For a selection without filtering" $
              queryWhere (req {queryString = []}) `shouldBe`
             ("", [])
        describe "Forming data for a query \"SELECT FROM post\" with ordering" $ do
            it "For a ordering by date" $
              queryOrder (req {queryString = [("order",Just "[date]")]})
              `shouldBe` (" ORDER BY draft_date", [])
            it "For a ordering by author" $
              queryOrder (req {queryString = [("order",Just "[author]")]})
              `shouldBe` (" ORDER BY user_name", [])
            it "For a ordering by category" $
              queryOrder (req {queryString = [("order",Just "[category]")]})
              `shouldBe` (" ORDER BY category_name", [])
            it "For a ordering by photo" $
              queryOrder (req {queryString = [("order",Just "[photo]")]})
              `shouldBe`
              (" ORDER BY (SELECT count (*) FROM getphotos WHERE d_id = draft_id)", [])
            it "For a ordering by all imaginable fields" $
              queryOrder (req {queryString = [
                       ("order",Just "[date, author, category, photo]")]})
              `shouldBe`
              (
               " ORDER BY draft_date, user_name, category_name, (SELECT count (*) FROM getphotos WHERE d_id = draft_id)"
              , [])
            it "Without ordering" $
              queryOrder (req {queryString = []}) `shouldBe` ("", [])

        



