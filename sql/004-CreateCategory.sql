CREATE TABLE category (
    category_id serial NOT NULL,
    category_name character varying (30) NOT NULL,
	owner_id integer,
    PRIMARY KEY (category_id),
	FOREIGN KEY (owner_id)
      REFERENCES category (category_id)
      ON DELETE CASCADE
);
