CREATE TABLE photo (
    photo_id serial NOT NULL,
    image text NOT NULL,
	image_type varchar (3) NOT NULL,
	CHECK
    ( image_type IN ( 'png', 'jpg', 'bmp', 'gif' )),
    PRIMARY KEY (photo_id)
);
