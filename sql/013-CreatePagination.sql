CREATE TABLE pagination(
    user_id integer NOT NULL,
    model character varying(30),
    part_query character varying(1024),
    PRIMARY KEY (user_id, model)	
);