CREATE TABLE author (
    user_id integer NOT NULL,
    description text NOT NULL,
    PRIMARY KEY (user_id),
	FOREIGN KEY (user_id)
      REFERENCES user_ (user_id)
      ON DELETE CASCADE
);
