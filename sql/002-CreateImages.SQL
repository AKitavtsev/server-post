CREATE TABLE images (
    id integer NOT NULL,
    image text NOT NULL,
	image_type varchar (4) NOT NULL,
	CHECK
    ( image_type IN ( 'png', 'jpg', 'bmp', 'gif' )),
    PRIMARY KEY (id),
	FOREIGN KEY (id)
      REFERENCES users (id)
      ON DELETE CASCADE
);
