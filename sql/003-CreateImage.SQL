DROP TABLE IF EXISTS image;
CREATE TABLE image (
    image_id serial NOT NULL,
    image bytea NOT NULL,
    PRIMARY KEY (image_id)
);