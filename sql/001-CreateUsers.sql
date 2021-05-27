DROP TABLE IF EXISTS users CASCADE;
CREATE TABLE users (
    id serial NOT NULL,
    name character varying(1024) NOT NULL,
    surname character varying(1024) NOT NULL,
    login character varying (1024) UNIQUE NOT NULL,
    password text NOT NULL,
    c_date timestamp NOT NULL,
    admin boolean NOT NULL,
    PRIMARY KEY (id)	
);
