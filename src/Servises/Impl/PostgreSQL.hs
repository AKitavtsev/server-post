{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Servises.Impl.PostgreSQL (
    newHandle,
) where

import qualified Servises.Config as C
import qualified Servises.Db as SD

import FromRequest
import Models.Author
import Models.Category
import Models.Comment
import Models.Draft
import Models.Post
import Models.Tag
import Models.User
import Servises.Impl.PostgreSQL.Internal
import Servises.Impl.PostgreSQL.Migrations

import Database.PostgreSQL.Simple

import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Data.Char (toLower)

import Data.Pool (Pool (..), withResource)
import Data.String (fromString)

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T


newHandle :: C.Config -> IO SD.Handle
newHandle config = do
    return $
        SD.Handle
            { SD.limit = C.limit config
            , SD.close = close
            , SD.newConn = newConn
            , SD.runMigrations = runMigrations
            , SD.deleteByID = deleteByID
            , SD.updateByID = updateByID
            , SD.insertUser = insertUser
            , SD.findUserByLogin = findUserByLogin
            , SD.findUserByID = findUserByID
            , SD.insertImage = insertImage
            , SD.findImageByID = findImageByID
            , SD.insertAuthor = insertAuthor
            , SD.findAuthorByID = findAuthorByID
            , SD.insertCategory = insertCategory
            , SD.findCategoryByID = findCategoryByID
            , SD.updateOwnerCategory = updateOwnerCategory
            , SD.insertTag = insertTag
            , SD.insertTagDraft = insertTagDraft
            , SD.insertPhotoDraft = insertPhotoDraft
            , SD.findTags = findTags
            , SD.insertDraft = insertDraft
            , SD.deleteDraft = deleteDraft
            , SD.updateDraft = updateDraft
            , SD.insertPhoto = insertPhoto
            , SD.findTagByID = findTagByID
            , SD.findPhotoByID = findPhotoByID
            , SD.findDraftByID = findDraftByID
            , SD.publishPost = publishPost
            , SD.insertComment = insertComment
            , SD.deleteComment = deleteComment
            , SD.findAllPosts = findAllPosts
            , SD.findComments = findComments
            }

newConn conf =
    connect
        defaultConnectInfo
            { connectUser = C.user conf
            , connectPassword = C.password conf
            , connectDatabase = C.name conf
            }

deleteByID pool model id = do
    case model of
        "user" ->
            liftIO $
                execSqlT
                    pool
                    [id]
                    "DELETE FROM user_ WHERE user_id=?"
        "author" ->
            liftIO $
                execSqlT
                    pool
                    [id]
                    "DELETE FROM author WHERE user_id=?"
        "category" ->
            liftIO $
                execSqlT
                    pool
                    [id]
                    "DELETE FROM category WHERE category_id=?"
        "tag" -> liftIO $ execSqlT pool [id] "DELETE FROM tag WHERE tag_id=?"
        "comment" -> liftIO $ execSqlT pool [id] "DELETE FROM comment WHERE id=?"
        "tag_draft" -> liftIO $ execSqlT pool [id] "DELETE FROM  tag_draft WHERE draft_id=?"
        "photo_draft" -> liftIO $ execSqlT pool [id] "DELETE FROM  photo_draft WHERE draft_id=?"
    return ()

updateByID pool model id fild value = do
    case model of
        "author" -> do
            liftIO $
                execSqlT
                    pool
                    [value, (show id)]
                    "UPDATE author SET description =? WHERE user_id=?"
        "tag" -> do
            liftIO $
                execSqlT
                    pool
                    [value, (show id)]
                    "UPDATE tag SET tag=? WHERE tag_id=?"
        "category" -> do
            liftIO $
                execSqlT
                    pool
                    [value, (show id)]
                    "UPDATE category SET category_name=? WHERE category_id=?"
    return ()

--User---------------------------------------------------------------------
insertUser pool (UserIn name surname avatar login password) c_date = do
    res <-
        liftIO $
            fetch
                pool
                [name, surname, login, password, c_date, "FALSE"]
                "INSERT INTO user_ (user_name, surname, login, password, user_date, admin) VALUES(?,?,?,md5( ?) ,?,?) returning user_id"
    return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

findUserByLogin pool login password = do
    res <-
        liftIO $
            fetch
                pool
                [login, password]
                "SELECT user_id, admin  FROM user_ WHERE login=? AND password = md5( ?)" ::
            IO [(Integer, Bool)]
    return $ pass res
  where
    pass [(id, adm)] = Just (id, adm)
    pass _ = Nothing

findUserByID pool id = do
    res <-
        liftIO $
            fetch
                pool
                (Only id)
                "SELECT user_name, surname, login, user_date::varchar, admin  FROM user_ WHERE user_id=?" ::
            IO [(String, String, String, String, Bool)]
    return $ pass res
  where
    pass [(n, sn, log, dat, adm)] =
        Just
            ( UserOut
                n
                sn
                ("http://localhost:3000/image/" ++ (show id))
                log
                dat
                adm
            )
    pass _ = Nothing

insertImage pool (UserIn name surname avatar login password) id = do
    case avatar of
        Just (Avatar im t) -> do
            res <-
                liftIO $
                    fetch
                        pool
                        [(show id), im, t]
                        "INSERT INTO image (user_id, image, image_type) VALUES (?,?,?) returning user_id"
            return $ pass res
          where
            pass [Only id] = id
            pass _ = 0
        Nothing -> return (-1)

findImageByID pool id = do
    res <-
        liftIO $
            fetch
                pool
                (Only id)
                "SELECT image, image_type FROM image WHERE user_id=?" ::
            IO [(String, String)]
    return $ pass res
  where
    pass [(img, t)] = Just (img, t)
    pass _ = Nothing

--Author------------------------------------------
insertAuthor pool (Author id description) = do
    res <-
        liftIO $
            fetch
                pool
                [show id, T.unpack description]
                "INSERT INTO author  (user_id, description) VALUES(?,?) returning user_id"
    return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

findAuthorByID pool id = do
    res <-
        liftIO $
            fetch
                pool
                (Only id)
                "SELECT user_name, surname, description FROM user_ INNER JOIN author USING(user_id) WHERE user_.user_id = ?;"
    return $ pass res
  where
    pass [(name, surname, descr)] =
        Just
            ( AuthorOut
                name
                surname
                descr
                ("http://localhost:3000/image/" ++ (show id))
            )
    pass _ = Nothing

--Category------------------------------
insertCategory pool (Category name owner_id) = do
    case owner_id of
        Just owner -> do
            res <-
                liftIO $
                    fetch
                        pool
                        [name, show owner]
                        "INSERT INTO category (category_name, owner_id) VALUES(?,?) returning category_id"
            return $ pass res
        Nothing -> do
            res <-
                liftIO $
                    fetch
                        pool
                        [name]
                        "INSERT INTO category (category_name) VALUES(?) returning category_id"
            return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

findCategoryByID pool id = do
    res <-
        liftIO $
            fetch
                pool
                (Only id)
                "SELECT * FROM category WHERE category_id=?" ::
            IO [(Integer, String, Maybe Integer)]
    return $ pass res
  where
    pass [(id, name, idOw)] = Just (Category name idOw)
    pass _ = Nothing

updateOwnerCategory pool id owner = do
    case map toLower owner of
        "null" -> do
            res <-
                liftIO $
                    fetch
                        pool
                        [(show id)]
                        "UPDATE category SET owner_id=null WHERE category_id=? returning category_id"
            return $ pass res
        _ -> do
            res <-
                liftIO $
                    fetch
                        pool
                        [owner, (show id)]
                        "UPDATE category SET owner_id=? WHERE category_id=? returning category_id"
            return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

--Tag-------------------------------------------------------------------------
insertTag pool (Tag tag) = do
    liftIO $
        execSqlT
            pool
            [tag]
            "INSERT INTO tag (tag) VALUES(?)"
    return ()

findTagByID pool id = do
    res <-
        liftIO $
            fetch
                pool
                (Only id)
                "SELECT * FROM tag WHERE tag_id=?" ::
            IO [(Integer, String)]
    return $ pass res
  where
    pass [(id, tag)] = Just (Tag tag)
    pass _ = Nothing

findTags pool id author = do
    res <-
        liftIO $
            fetch
                pool
                [id, author]
                "SELECT tag FROM tag WHERE array_position ((SELECT tags FROM drafts WHERE id = ? AND author = ?), id) IS NOT NULL"
    return $ pass res
  where
    pass [] = []
    pass xs = map fromOnly xs

--Draft------------------------------------------------------------------------
insertDraft
    pool
    (DraftIn title category tags t_content mainPhoto otherPhotos)
    id
    c_date = do
        res <-
            liftIO $
                fetch
                    pool
                    [ title
                    , c_date
                    , show id
                    , show category
                    , T.unpack t_content
                    , show mainPhoto
                    ]
                    "INSERT INTO draft (title, draft_date, user_id, category_id, t_content, photo_id) VALUES(?,?,?,?,?,?) returning draft_id"
        return $ pass res
      where
        pass [Only id] = id
        pass _ = 0

deleteDraft pool id author = do
    liftIO $ execSqlT pool [id, author] "DELETE FROM draft WHERE draft_id=? AND user_id =?"
    return ()

updateDraft pool draft author = do
    case partQuery draft of
        "" -> return (Just draft)
        _ -> do
            res <-
                liftIO $
                    fetch pool [id_draft draft, author] $
                        fromString
                            ( "UPDATE draft SET" ++ partQuery draft
                                ++ " WHERE draft_id=? AND user_id =? returning title, category_id, t_content, photo_id"
                            )
            return $ pass res
  where
    pass [(t, c, tc, p)] =
        Just
            ( draft
                { newTitle = newValue (newTitle draft) t
                , newCategory = newValue (newCategory draft) c
                , newContent = newValue (newContent draft) tc
                , newMainPhoto = newValue (newMainPhoto draft) p
                }
            )
    pass _ = Nothing
    newValue a na = if a == Nothing then Nothing else Just na

insertTagDraft pool id tag = do
    res <-
        liftIO $
            fetch
                pool
                [show tag, show id]
                "INSERT INTO tag_draft (tag_id, draft_id) VALUES(?,?) returning tag_id"
    return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

insertPhotoDraft pool id photo = do
    res <-
        liftIO $
            fetch
                pool
                [show photo, show id]
                "INSERT INTO photo_draft (photo_id, draft_id) VALUES(?,?) returning photo_id"
    return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

findDraftByID pool id_d = do
    res <-
        liftIO $
            fetch
                pool
                [id_d]
                "SELECT title, draft_date :: varchar, category_id, ARRAY (SELECT tag_id FROM tag_draft WHERE draft.draft_id=tag_draft.draft_id) :: varchar, photo_id  :: varchar, ARRAY (SELECT photo_id FROM photo_draft WHERE draft.draft_id=photo_draft.draft_id):: varchar, t_content FROM draft WHERE draft.draft_id = ?" ::
            IO [(String, String, Integer, String, String, String, T.Text)]
    return $ pass res
  where
    pass [(title, c_date, id_cat, tags, photo, photos, t_content)] =
        Just
            ( DraftGet
                title
                c_date
                id_cat
                (toListInteger tags)
                ("http://localhost:3000/photo/" ++ photo)
                (map fromPhotoId (toListInteger photos))
                t_content
            )
    pass _ = Nothing

publishPost pool draft author = do
    liftIO $ execSqlT pool [draft, author] "DELETE FROM post WHERE draft_id=? AND user_id =?"
    res <-
        liftIO $
            fetch
                pool
                [draft, author]
                "INSERT INTO post (draft_id, title, draft_date, user_id, category_id, photo_id, t_content) SELECT * FROM draft WHERE draft_id=? AND user_id=? returning draft_id"
    return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

--Photo---------------------------------------------------------------------------------
insertPhoto pool (Photo im t) = do
    res <-
        liftIO $
            fetch
                pool
                [im, t]
                "INSERT INTO photo (image, image_type) VALUES (?,?) returning photo_id"
    return $ pass res
  where
    pass [Only id] = id
    pass _ = 0

findPhotoByID pool id = do
    res <-
        liftIO $
            fetch
                pool
                (Only id)
                "SELECT image, image_type FROM photo WHERE photo_id=?" ::
            IO [(String, String)]
    return $ pass res
  where
    pass [(img, t)] = Just (img, t)
    pass _ = Nothing

--Comment----------------------------------------------------------------------
insertComment
    pool
    (CommentIn post_id comment)
    author_id
    c_date = do
        res <-
            liftIO $
                fetch
                    pool
                    [ c_date
                    , show post_id
                    , comment
                    , show author_id
                    ]
                    "INSERT INTO comment (comment_date, draft_id, comment, user_id) VALUES(?,?,?,?) returning comment_id"
        return $ pass res
      where
        pass [Only id] = id
        pass _ = 0

deleteComment pool id author = do
    liftIO $ execSqlT pool [id, author] "DELETE FROM comments WHERE id=? AND user_id =?"
    return ()

-- Post------------------------------------------------------------------------
findAllPosts pool req limit id = do
    endQuery <- queryWhereOrder pool req limit id
    let query =
            "WITH "
                ++ "gettags (t_id, t_name, d_id)"
                ++ " AS (SELECT tag_id, tag, draft_id"
                ++ " FROM tag_draft"
                ++ " INNER JOIN tag USING (tag_id)"
                ++ " INNER JoIN post USING (draft_id)),"
                ++ "getphotos (p_id, d_id)"
                ++ " AS (SELECT photo_draft.photo_id, draft_id"
                ++ " FROM photo_draft"
                ++ " INNER JOIN post USING (draft_id)) "
                ++ "SELECT draft_id, title,"
                ++ " draft_date :: varchar,"
                ++ " category_name, category_id,"
                ++ " user_id::varchar , user_name, surname, description,"
                ++ " ARRAY (SELECT t_name"
                ++ " FROM gettags"
                ++ " WHERE d_id = draft_id):: varchar,"
                ++ " photo_id  :: varchar,"
                ++ " ARRAY (SELECT p_id"
                ++ " FROM getphotos"
                ++ " WHERE d_id = draft_id):: varchar,"
                ++ " t_content "
                ++ "FROM post"
                ++ " INNER JOIN user_ USING (user_id)"
                ++ " INNER JOIN author USING (user_id)"
                ++ " INNER JOIN category USING (category_id)"
                ++ endQuery
    res <- fetchSimple pool $ fromString query
    mapM toPost res
  where
    toPost
        ( id
            , title
            , c_date
            , category
            , category_id
            , user_id
            , user_name
            , surname
            , description
            , tags
            , photo
            , photos
            , t_content
            ) = do
            subcat <- allCategories [category_id] []
            subcatName <- mapM getNameSC subcat
            return
                ( Post
                    id
                    title
                    c_date
                    ( AuthorOut
                        user_name
                        surname
                        description
                        ("http://localhost:3000/image/" ++ user_id)
                    )
                    (Categories category subcatName)
                    (toListString tags)
                    ("http://localhost:3000/photo/" ++ photo)
                    (map fromPhotoId (toListInteger photos))
                    t_content
                )
    allCategories :: [Integer] -> [Integer] -> IO [Integer]
    allCategories [] tg = return tg
    allCategories xs tg = do
        subxs <- mapM getSubCat xs
        allCategories (concat subxs) (tg ++ (concat subxs))
      where
        getSubCat id = findSubCat pool id
    findSubCat pool id = do
        res <-
            liftIO $
                fetch
                    pool
                    (Only id)
                    "SELECT category_id FROM category WHERE  owner_id=?"
        return (map fromOnly res)
    findNameSC pool id = do
        res <-
            liftIO $
                fetch
                    pool
                    (Only id)
                    "SELECT category_name FROM category WHERE  category_id=?"
        return $ pass res
      where
        pass [Only name] = name
        pass _ = ""
    getNameSC id = findNameSC pool id

findComments pool req limit id_post = do
    let offset = case toParam req "page" of
            Nothing -> 0
            Just page -> limit * (read' page - 1)
        query =
            "SELECT comment_date :: varchar,"
                ++ " user_id ::varchar, user_name, surname, comment "
                ++ "FROM comment INNER JOIN user_ USING (user_id) "
                ++ "WHERE draft_id = "
                ++ show id_post
                ++ " LIMIT "
                ++ show limit
                ++ " OFFSET "
                ++ show offset
    res <- fetchSimple pool $ fromString query
    return (map toComment res)
  where
    toComment (comment_data, user_id, user_name, surname, comment) =
        Comment
            comment_data
            ( User
                user_name
                surname
                ("http://localhost:3000/image/" ++ user_id)
            )
            comment
