-- test.sql
BEGIN;
SELECT plan(1);

SELECT pass('Hello');
SELECT fail('Hello');

SELECT * FROM finish(TRUE);
ROLLBACK;
