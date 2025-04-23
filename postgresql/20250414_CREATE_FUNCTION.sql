ROLLBACK;
BEGIN;

drop type if exists mytype;

create type mytype AS (name varchar, value int);

CREATE OR REPLACE FUNCTION pg_temp.fff(mytype) RETURNS int AS $$
  select $1.value;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pg_temp.add (x int = 1, y int = 2) RETURNS int AS $$
  select x + add.x + $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pg_temp.add2 (x int = 1, y int = 2, out result int) AS $$
  select x + add2.x + $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION add3 (x int = 1, y int = 2, out result int, out z int) RETURNS RECORD AS $$
  select x + x + $2, x * 2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pg_temp.setup () RETURNS void AS $$
  create table a (value int primary key generated always AS identity);
  -- insert into a default values;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pg_temp.g() RETURNS setof pg_settings AS
  'select * from pg_settings'
  LANGUAGE SQL;
CREATE OR REPLACE FUNCTION pg_temp.g2(out text, out category text) RETURNS setof record AS
  'select name, category from pg_settings'
  LANGUAGE SQL;

-- CREATE OR REPLACE FUNCTION k() RETURNS table(x int, y text) AS $$
CREATE OR REPLACE FUNCTION pg_temp.k(out x int, out y text) RETURNS setof record AS $$
  select x, z AS y from (values (1, 'a'), (2, 'b')) t(x, z);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pg_temp.k2(out x int, out y text) AS $$
  select x, z AS y from (values (1, 'c'), (2, 'b')) t(x, z);
$$ LANGUAGE SQL;

CREATE OR REPLACE PROCEDURE pg_temp.clear_functions(namespace text DEFAULT 'public') AS $$
DECLARE
  func record;
BEGIN
  FOR func IN
      SELECT *
      FROM pg_proc pro
      INNER JOIN pg_namespace n ON pro.pronamespace = n.oid
      WHERE
        n.nspname = namespace
        AND pro.proname <> 'clear_functions'
  LOOP
      RAISE NOTICE 'A';
      EXECUTE format('DROP ROUTINE %I', func.proname);
  END LOOP;
END;
$$ LANGUAGE PLPGSQL;
