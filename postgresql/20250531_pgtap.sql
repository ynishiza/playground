BEGIN;
SELECT plan(11);

SELECT pass('hello');

SELECT ok(true, 'OK');

SELECT is(1, 1, '1=1');
-- SELECT is(1, 2, '1=2');
SELECT isnt(1, 2, '1!=2');

SELECT matches('ABC'::text, '^A', 'is ABC');
SELECT doesnt_match('DEF'::text, '^A', 'is not ABC');
SELECT imatches('ABC'::text, '^A');

SELECT cmp_ok(1, '<', 2, '1<2');
SELECT cmp_ok(2, '>', 1, '1>2');

SELECT isa_ok(1, 'integer');
SELECT isa_ok(1::numeric, 'numeric');
-- SELECT pass('hello');
--
SELECT * FROM finish(true);
ROLLBACK;

BEGIN;
CREATE OR REPLACE function test_abc() RETURNS SETOF TEXT AS $$
  SELECT pass('fn');
$$ LANGUAGE SQL;

SELECT * FROM runtests();

ROLLBACK;
