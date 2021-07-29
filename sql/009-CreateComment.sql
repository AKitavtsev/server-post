CREATE TABLE comment (
    comment_id serial,
	comment_date timestamp NOT NULL,
	draft_id integer NOT NULL,
    comment character varying(1024) NOT NULL,
	user_id integer NOT NULL,
    PRIMARY KEY (comment_id),
	FOREIGN KEY (user_id)
      REFERENCES user_ (user_id)
      ON DELETE CASCADE,
	FOREIGN KEY (draft_id)
      REFERENCES post (draft_id)
);
