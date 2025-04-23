DEALLOCATE all;

PREPARE getCount (int) AS
  SELECT * FROM pg_class LIMIT $1;

EXECUTE getCount(10);

PREPARE hello AS
  SELECT 'hello';
