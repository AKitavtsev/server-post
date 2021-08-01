WITH
gettags (t_id, t_name, d_id)
  AS (SELECT tag_id, tag, draft_id)
      FROM tag_draft
           INNER JOIN tag USING (tag_id)
	       INNER JoIN post USING (draft_id)),
getphotos (p_id, d_id )
  AS (SELECT photo_draft.photo_id, photo_draft.draft_id
      FROM photo_draft
           INNER JOIN post USING (draft_id))
SELECT draft_id, title,
       draft_date :: varchar,
       category_name,
	   user_name, surname, description,
       ARRAY (SELECT t_name FROM gettags WHERE d_id = draft_id) AS tags_names,
       photo_id  :: varchar,
       ARRAY (SELECT p_id FROM getphotos WHERE d_id = draft_id) AS photos_ids
FROM post
     INNER JOIN user_ USING (user_id)
	 INNER JOIN author USING (user_id)
	 INNER JOIN category USING (category_id)
	 
	 
WHERE array_position (ARRAY (SELECT tags_ids FROM gettags), 2) IS NOT NULL
ORDER BY (SELECT count (*) FROM getphotos);