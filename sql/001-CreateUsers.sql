DROP TABLE IF EXISTS users;
CREATE TABLE users (
    user_id serial NOT NULL,
    name character varying(1024) NOT NULL,
    surname character varying(1024) NOT NULL,
    avatar character varying (1024),
    login character varying (1024) UNIQUE,  
    password text NOT NULL,
    c_date timestamp NOT NULL,
    admin boolean NOT NULL,
    PRIMARY KEY (user_id)	
);