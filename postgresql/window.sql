DROP TABLE IF EXISTS numbers, numbers_repeated;

CREATE TABLE numbers (
  n INTEGER NOT NULL,
  is_even BOOLEAN GENERATED ALWAYS AS (n % 2 = 0) STORED
);
INSERT INTO numbers (n) (SELECT generate_series(1, 50));

CREATE TABLE numbers_repeated ( n INTEGER NOT NULL);
INSERT INTO numbers_repeated (n)
  (SELECT x FROM
    (SELECT x, generate_series(1,x) FROM generate_series(1, 10) AS x) _
  );

SELECT 
  n, 
  array_agg(n) OVER w AS window,
  ROW_NUMBER() OVER w,
  RANK() OVER w,
  DENSE_RANK() OVER w
  FROM numbers 
  -- WHERE n % 5 != 0
  WINDOW w AS (
    -- PARTITION BY n % 5
    -- PARTITION BY n % 2 ORDER BY n % 3
    PARTITION BY n % 2 
    ORDER BY n % 3 
    -- GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
    -- GROUPS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
    GROUPS BETWEEN 2 PRECEDING AND 0 FOLLOWING
    -- GROUPS CURRENT ROW
  )
  ORDER BY n;

SELECT (t).numbers.*
FROM (
  SELECT 
    ROW_NUMBER() OVER (ORDER BY n) as r, 
    numbers FROM numbers
) t
WHERE t.r % 3 = 0
