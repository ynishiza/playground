-- \if off
drop routine if exists temp;
drop table if exists t1;

select column1 as id, column2 as t into t1 from (values (1, 'a'), (2, 'b'));


set plpgsql.extra_errors='none';
set plpgsql.extra_warnings='all';

CREATE OR REPLACE PROCEDURE f(x int) AS $$
<<b>>
DECLARE
  x int := 10;
BEGIN
  RAISE INFO '
x=%
f(x)=%
b.x=%
', x, f.x, b.x;
END;
$$ LANGUAGE PLPGSQL;

call f(-1);

create or replace procedure temp() AS $$
declare
  x int = 1;
  y int = 1;
  z int = 1;
  err_schema text;
  err_hint text;
begin
  values (10, 20), (3,4) into x, y, z;

  raise info '% % %', x, y, FOUND;
  RAISE EXCEPTION 'oops' USING
    SCHEMA = 'abc',
    HINT = 'something';

  EXCEPTION
    WHEN OTHERS THEN
      GET STACKED DIAGNOSTICS
        err_schema := SCHEMA_NAME,
        err_hint := PG_EXCEPTION_HINT;
      RAISE INFO 'schema:%  hint:%', err_schema, err_hint;
end;
$$ language plpgsql;

call temp();

