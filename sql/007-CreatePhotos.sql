CREATE TABLE photos (
    id serial,
    image text NOT NULL,
	image_type varchar (4) NOT NULL,
	author integer NOT NULL,
	CHECK
    ( image_type IN ( 'png', 'jpg', 'bmp', 'gif' )),
    PRIMARY KEY (id),
	FOREIGN KEY (author)
      REFERENCES authors (id)
      ON DELETE CASCADE
);
