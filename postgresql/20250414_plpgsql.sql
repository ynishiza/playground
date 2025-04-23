ROLLBACK;
BEGIN;

DROP TABLE IF EXISTS tempt CASCADE;

SELECT column1 as id, column2 as value
  INTO TEMPORARY tempt
  FROM (VALUES (1, 'a'), (2, 'b'));

CREATE OR REPLACE FUNCTION test_declaration_and_assignment(
  text
) RETURNS INT AS $$
DECLARE
  x INT := 1;
  t ALIAS FOR $1;
  vals record;
  y x%TYPE := 2;
  c CONSTANT INT = 10;
BEGIN
  SELECT 5 INTO x;
  INSERT INTO tempt VALUES (100, 'c') RETURNING tempt.id INTO vals;
  RETURN x + y + CAST(t as int) + c;
END;
$$ LANGUAGE plpgsql;

--
CREATE OR REPLACE PROCEDURE test_raise() AS $$
BEGIN
  RAISE INFO USING MESSAGE = format('abc %s', 1);
  RAISE INFO 'def % %', 1, TRUE;
  RAISE WARNING SQLSTATE '00011';
  RAISE WARNING division_by_zero;
  RAISE EXCEPTION 'OOPS' USING
    DETAIL = 'something is wrong',
    HINT = 'check lin 1',
    SCHEMA = 'publics',
    DATATYPE = 'mytype';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION simple2(RECORD) RETURNS RECORD AS $$
BEGIN
  RETURN $1;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION h() returns setof pg_class as $$
declare
  view pg_class;
begin
  for view in
    select (c).* from pg_class c
    inner join pg_namespace nsp on c.relnamespace = nsp.oid
    where  nsp.nspname = 'public'
  loop
    return next view;
    -- return next view;
  end loop;
  return;
end;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE PROCEDURE test_out_inner(a int, out x int, out y text, out z boolean) as $$
BEGIN
    x := a + 1;
    y := 'hello';
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE PROCEDURE test_out_outer() as $$
DECLARE
  x int;
  y text;
  z boolean;
BEGIN
  CALL test_out_inner(100, x, y, z);
  RAISE INFO '% % %', x, y, z;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION h2() returns setof name as $$
declare
  view pg_class;
begin
  for view in select (c).* from pg_class c
  loop
    return next view.relname;
  end loop;
  return;
end;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION pg_temp.h3() returns TABLE(n name, i oid) as $$
declare
  view record;
begin
  for view in select (c).* from pg_class c
  loop
    n := view.relname;
    i := view.oid;
    return next;
  end loop;
  return;
end;
$$ LANGUAGE plpgsql;

-- CREATE OR REPLACE FUNCTION pg_temp.h4(out n name, out i oid) returns setof record as $$
CREATE FUNCTION pg_temp.h4(out n name, out i oid) returns setof record as $$
declare
  view record;
begin
  for view in select (c).* from pg_class c
  loop
    n := view.relname;
    i := view.oid;
    return next;
  end loop;
  return;
end;
$$ LANGUAGE plpgsql;

commit;
