CREATE TABLE draft (
    draft_id    serial NOT NULL,
    title       character varying(200) NOT NULL,
	draft_date  timestamp NOT NULL,
	user_id     integer NOT NULL,
	category_id integer NOT NULL,
    photo_id    integer NOT NULL,
    t_content   text NOT NULL,	
    PRIMARY KEY (draft_id),
	FOREIGN KEY (user_id)
      REFERENCES author (user_id)
	  ON DELETE CASCADE,
	FOREIGN KEY (category_id)
      REFERENCES category (category_id),
	FOREIGN KEY (photo_id)
      REFERENCES photo (photo_id)
);
