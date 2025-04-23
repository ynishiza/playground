DROP TABLE IF EXISTS tablea;

SELECT * FROM
SELECT * FROM
DROP ROLE IF EXISTS testrole;
CREATE ROLE testrole WITH LOGIN;

CREATE TABLE tablea (
  id INT GENERATED ALWAYS AS IDENTITY,
  value VARCHAR DEFAULT '',
  value2 BOOLEAN DEFAULT FALSE
);

-- REVOKE SELECT, INSERT on tablea FROM testrole;
GRANT SELECT (value2), INSERT (value) on tablea TO testrole;


DROP SCHEMA IF EXISTS abcschema CASCADE;
DROP ROLE IF EXISTS abcperson;

CREATE ROLE abcperson WITH
  LOGIN
  PASSWORD 'abc';

CREATE SCHEMA abcschema
  CREATE TABLE abctable (
    id INT GENERATED ALWAYS AS IDENTITY,
    value VARCHAR DEFAULT ''
  )
  ;
GRANT SELECT, INSERT(value), UPDATE(value) ON abcschema.abctable TO abcperson;

ALTER ROLE abcperson SET search_path=abcschema;
