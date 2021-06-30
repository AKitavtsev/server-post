CREATE TABLE posts (
    id        integer NOT NULL,
    title     character varying(1024),
	c_date    timestamp NOT NULL,
	author    integer NOT NULL,
	category  integer NOT NULL,
	tags      integer [],
	photo     integer,
	photos    integer [],     
    t_content text,	
    PRIMARY KEY (id)
);
