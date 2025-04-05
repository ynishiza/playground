DROP SCHEMA IF EXISTS abcschema CASCADE;
DROP ROLE IF EXISTS abcadmin, abcuser;

-- user: admin
CREATE ROLE abcadmin
  WITH 
    CREATEROLE
    LOGIN
    PASSWORD 'abc';
ALTER ROLE abcadmin SET search_path=abcschema;

CREATE SCHEMA abcschema
  AUTHORIZATION abcadmin

  CREATE TABLE abctable (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY (START 10),
    x INT NOT NULL,
    y INT NOT NULL DEFAULT 10,
    z INT GENERATED ALWAYS AS (y * x) STORED
  );


-- user: normal user
CREATE ROLE abcuser
  WITH 
    LOGIN
    PASSWORD 'abc';

-- set default schema
ALTER ROLE abcuser SET search_path=abcschema;       
-- allow access to schema
GRANT USAGE ON SCHEMA abcschema TO abcuser;
-- set permissions
GRANT SELECT, INSERT(x), UPDATE(x) ON abcschema.abctable TO abcuser;

SET ROLE abcuser;
INSERT INTO abcschema.abctable (x) VALUES (1), (2), (10);
