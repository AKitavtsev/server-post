DROP TABLE IF EXISTS images;
CREATE TABLE images (
    image_id serial NOT NULL,
    image text NOT NULL,
	image_type varchar (4) NOT NULL, 
    PRIMARY KEY (image_id)
);