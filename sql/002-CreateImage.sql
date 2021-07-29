CREATE TABLE image (
	user_id integer NOT NULL,
    image text NOT NULL,
	image_type varchar (3) NOT NULL,
	CHECK
    ( image_type IN ( 'png', 'jpg', 'bmp', 'gif' )),
    PRIMARY KEY (user_id),
	FOREIGN KEY (user_id)
      REFERENCES user_ (user_id)
      ON DELETE CASCADE
);
