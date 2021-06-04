CREATE TABLE categories (
    id serial NOT NULL,
    name character varying (1024) NOT NULL,
	id_owner integer,
    PRIMARY KEY (id),
	FOREIGN KEY (id_owner)
      REFERENCES categories (id)
      ON DELETE CASCADE
);
