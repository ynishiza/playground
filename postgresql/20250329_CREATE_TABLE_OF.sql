ROLLBACK;
BEGIN;

DROP TYPE IF EXISTS usertype CASCADE;
CREATE TYPE usertype as (
  firstname varchar,
  lastname varchar
);

CREATE TEMPORARY TABLE user_table OF usertype (
  CONSTRAINT value_pk PRIMARY KEY(firstname, lastname),
  lastname UNIQUE
);

INSERT INTO user_table VALUES ('abc', 'def');

INSERT INTO user_table (
  SELECT format('n_%s', n), format('u_%s', n) as lastname
  FROM generate_series(1, 1000) t(n)
);

COMMIT;
