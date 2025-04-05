DROP DOMAIN IF EXISTS md, ms;
DROP TYPE IF EXISTS mt;

CREATE DOMAIN md as INT;
CREATE DOMAIN ms as CHAR(2);
CREATE TYPE mt as (
  x INT,
  v VARCHAR
);

select
 '==== BOOLEAN ====',
 cast(1 AS BOOLEAN),
 cast(0 AS BOOLEAN),
 -- cast(1.0 AS BOOLEAN),

 cast('t' AS BOOLEAN),
 cast('true' AS BOOLEAN),
 cast('1' AS BOOLEAN),
 -- cast('a' AS BOOLEAN),
 -- cast(date '2025-01-01' AS BOOLEAN),
 -- cast(row(1) AS BOOLEAN),

  '==== string ====',
  cast(1 AS VARCHAR),
  cast(123456 AS VARCHAR(2)),
  cast(1.23456 AS VARCHAR(2)),
  cast(TRUE AS VARCHAR),
  cast(TRUE AS VARCHAR(2)),
  cast(CURRENT_TIMESTAMP AS VARCHAR(2)),
  cast((1, '')::mt AS VARCHAR(2)),
  cast(row(1, '') AS VARCHAR(2)),

  '==== numeric ====',
  cast(TRUE as INT),
  -- cast(TRUE as NUMERIC),
  -- cast(TRUE as REAL),
  cast('1' as INT),
  -- cast('1.1' as INT),
  cast('1.123456789' as REAL),
  cast('1.123456' as NUMERIC(4,2)),

  '==== END ===='
\gx
 -- '==== numeric ====',
 -- cast(1::bigint as smallint),
 -- cast(1.0::float as smallint),
 -- cast(1.1::float as NUMERIC(4, 2)),
 -- -- cast(123.123::float as NUMERIC(4, 2)),
 -- -- cast(0.123456789::numeric as NUMERIC(2, 2)),
 -- cast(10.234::float as smallint),
 -- cast(1.0::numeric as smallint),
 -- cast(123456789::numeric as real),
 -- cast(0.123456789::numeric as real),
 -- '==== Integer ====',
 -- cast(1::int as numeric),
 -- cast(100000000000::bigint as float(2)),
 -- -- cast(100000000000::bigint as numeric(2)),
 -- -- cast(100000000::int as smallint), out of range
 -- cast(1::int as varchar),
 -- cast(1000::int as varchar(2)),
 -- cast(1::int as char(10)),
 -- cast(1::int as boolean), -- T
 -- cast(0::int as boolean), -- F
 -- cast(-1::int as boolean), -- T
 -- -- cast(1::int as date),
 -- cast(1::int as md),
 -- -- cast(1::int as mt),
 -- cast(1::int as ms),
 -- cast(1000::int as ms),
 -- -- cast(1::int as mt),

 -- '==== Real ====',
 -- cast(123456789.123456789::real as int),
 -- cast(1.1::real as smallint),
 -- -- cast(123456789.123456789::real as smallint),
 -- cast(123456789.123456789::float as numeric),
 -- -- cast(123456789.123456789::float as numeric(2)),
 -- cast(123456789.123456789::numeric as float(1)),
 -- cast(123456789.123456789::numeric as float(2)),
 -- cast(123456789.123456789::numeric as real),
 -- cast(123456789.123456789::numeric as double precision), -- T
 -- -- cast(123456789.123456789::numeric as numeric(1)),
 -- -- cast(1.1::real as boolean), -- T
 -- -- cast(0::real as boolean), -- F
 -- cast(123456789.123456789::numeric as VARCHAR), -- T
 -- cast(123456789.123456789::numeric as VARCHAR(2)), -- T
 -- cast(123456789.123456789::real as md),

 -- '==== BOOLEAN ====',
 -- cast(TRUE as INT),
 -- cast(FALSE as INT),
 -- -- cast(TRUE as REAL),
 -- -- cast(TRUE as numeric) is null,
 -- cast(TRUE as varchar),
 -- cast(TRUE as varchar(1)),
 -- -- cast(TRUE as date),

 -- '==== String ====',
 -- -- cast('a' as numeric),
 -- cast('true' as boolean),
 -- cast('t' as boolean),
 -- -- cast('a' as boolean),
 -- cast('1.1' as numeric),
 -- cast('1' as int),
 -- -- cast('a' as int),
 -- -- cast('a' as date),
 -- cast('01-01-2025' as date),
 -- cast('01:10:30.1234567 + 09' as time),
 -- cast('01:10:30.1234567 + 09' as time with time zone),

 --  '==== Date ====',
 -- cast(date '01-20-2025' as varchar),
 -- -- cast(date '01-20-2025' as numeric),
 -- cast(date '01-20-2025' as varchar),
 -- -- cast(date '01-20-2025' as time),
 -- cast(date '01-20-2025' as timestamp),

 -- cast(NULL as numeric) is null,
 -- cast(NULL as boolean) is null,
 -- cast(NULL as varchar) is null,
 -- cast(NULL as md) is null,
 -- cast(NULL as mt) is null,

 -- cast('1' as md),
 -- cast('(1, "a")' as mt),

 --  '==== END ===='
-- \gx
