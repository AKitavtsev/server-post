CREATE TABLE photo_draft (
    photo_draft_id serial NOT NULL,
    photo_id integer NOT NULL,
    draft_id integer NOT NULL,
    PRIMARY KEY (photo_draft_id),
    FOREIGN KEY (photo_id)
      REFERENCES photo (photo_id)
      ON DELETE CASCADE,
	FOREIGN KEY (draft_id)
      REFERENCES draft (draft_id)
      ON DELETE CASCADE
);
