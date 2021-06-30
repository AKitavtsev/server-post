CREATE TABLE comments (
    id serial,
	c_date timestamp NOT NULL,
	post_id integer NOT NULL,
    comment character varying(1024) NOT NULL,
	user_id integer NOT NULL,
    PRIMARY KEY (id),
	FOREIGN KEY (user_id)
      REFERENCES users (id)
      ON DELETE CASCADE,
	FOREIGN KEY (post_id)
      REFERENCES posts (id)
);
