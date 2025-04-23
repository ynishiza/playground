DROP SEQUENCE IF EXISTS myseq, mycycle;

CREATE SEQUENCE myseq AS INT 
  INCREMENT BY 3
  MINVALUE 1 
  MAXVALUE 8
  START 1;

CREATE SEQUENCE mycycle AS INT 
  INCREMENT BY 5
  MAXVALUE 10
  CYCLE;

  -- SELECT currval('myseq');
  SELECT nextval('myseq');
  SELECT currval('myseq');
  SELECT lastval();
  SELECT nextval('myseq');
  SELECT lastval();

  -- SELECT setval('myseq', 3);
  SELECT setval('myseq', 1, false);
  SELECT currval('myseq');
  SELECT nextval('myseq');

  SELECT nextval('mycycle')
    FROM generate_series(1, 5);
