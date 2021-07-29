CREATE TABLE tag_draft (
    tag_draft_id serial NOT NULL,
    tag_id integer NOT NULL,
    draft_id integer NOT NULL,
    PRIMARY KEY (tag_draft_id),
    FOREIGN KEY (tag_id)
      REFERENCES tag (tag_id)
      ON DELETE CASCADE,
	FOREIGN KEY (draft_id)
      REFERENCES draft (draft_id)
      ON DELETE CASCADE
);
