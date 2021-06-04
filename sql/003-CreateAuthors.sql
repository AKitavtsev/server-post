CREATE TABLE authors (
    id integer NOT NULL,
    description text NOT NULL,
    PRIMARY KEY (id),
	FOREIGN KEY (id)
      REFERENCES users (id)
      ON DELETE CASCADE
);
