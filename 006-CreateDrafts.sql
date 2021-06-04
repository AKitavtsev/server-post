DROP TABLE IF EXISTS drafts;
CREATE TABLE drafts (
    id            serial NOT NULL,
    title         character varying(1024),
	c_date        timestamp NOT NULL,
	author        integer NOT NULL,
	category      integer NOT NULL,
	tags          integer [],
    t_content     text,	
    PRIMARY KEY (id),
	FOREIGN KEY (author)
      REFERENCES authors (id),
	FOREIGN KEY (category)
      REFERENCES categories (id),
);
