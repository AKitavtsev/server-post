DROP TABLE IF EXISTS images;
CREATE TABLE images (
    image_id integer NOT NULL,
    image text NOT NULL,
	image_type varchar (4) NOT NULL,
	CHECK
    ( image_type IN ( 'png', 'jpg', 'bmp', 'gif' )),
    PRIMARY KEY (image_id),
	FOREIGN KEY (image_id)
      REFERENCES users (user_id)
      ON DELETE CASCADE
);
