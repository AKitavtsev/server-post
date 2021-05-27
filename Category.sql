DROP TABLE IF EXISTS categories;
CREATE TABLE categories (
    id serial NOT NULL,
    name character varying(1024) NOT NULL,
	id_owner integer,
    PRIMARY KEY (id),
	FOREIGN KEY (id_owner)
      REFERENCES categories (id)
      ON DELETE CASCADE
);

INSERT INTO categories (name) VALUES ('111');

INSERT INTO categories (name, id_owner) VALUES ('1.3', 3);

SELECT id, id_owner FROM categories WHERE id = 1;

SELECT id, COALESCE(id_owner, 0) id_owner FROM categories WHERE id = 1;