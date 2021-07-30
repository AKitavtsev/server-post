SELECT draft_id, title,
       draft_date :: varchar,
       category_name,
	   user_name, surname, description,
       ARRAY (SELECT tag 
	          FROM tag_draft
                   INNER JOIN tag USING (tag_id)	 		  
	          WHERE post.draft_id=tag_draft.draft_id),
       photo_id  :: varchar,
       ARRAY (SELECT photo_id FROM photo_draft WHERE post.draft_id=photo_draft.draft_id),
	   t_content
FROM post
     INNER JOIN user_ USING (user_id)
	 INNER JOIN author USING (user_id)
	 INNER JOIN category USING (category_id);