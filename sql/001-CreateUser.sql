CREATE TABLE user_ (
    user_id serial NOT NULL,
    user_name character varying(30) NOT NULL,
    surname character varying(30) NOT NULL,
    login character varying (30) UNIQUE NOT NULL,
    password text NOT NULL,
    user_date timestamp NOT NULL,
    admin boolean NOT NULL,
    PRIMARY KEY (user_id)	
);
