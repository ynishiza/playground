DROP DOMAIN IF EXISTS largevalue;
DROP TYPE IF EXISTS mypoint;

CREATE DOMAIN largevalue AS INT
  CHECK (VALUE > 100);

CREATE TYPE mypoint AS (
  x INT,
  y INT
)
