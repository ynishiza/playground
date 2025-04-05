select
n,
array_agg(n) OVER w
-- ROW_NUMBER() OVER w,
-- RANK() OVER w,
-- DENSE_RANK() OVER w
-- from numbers_repeated_rev
-- from numbers_repeated_rev
from (SELECT * FROM numbers LIMIT 30) as x
WINDOW w AS (PARTITION BY n % 5 ORDER BY n)
-- WINDOW w AS (ORDER BY n % 3)
-- WINDOW w AS (ORDER BY n)
-- WINDOW w AS (ORDER BY n)
-- WINDOW w AS (PARTITION BY n % 5 ORDER BY n)
-- WINDOW w AS (PARTITION BY n % 5)
-- WINDOW w AS (PARTITION BY n ORDER BY n)
ORDER BY n
;
