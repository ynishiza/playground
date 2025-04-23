ROLLBACK;
\set SINGLESTEP on
set enable_async_append=off;
set enable_parallel_append=off;

drop table if exists t_dep, t_ndis;
drop statistics if exists st_dep, st_ndis;
deallocate all;

prepare check_stats as
  select
    stxname,
    stxkind,
    stxkeys,
    stxd.*
  from pg_statistic_ext stx
  inner join pg_statistic_ext_data stxd
  on stx.oid = stxd.stxoid;

\echo 'functional dependency'
select x a, x % 5 as b, x % 10 as c
into table t_dep
from generate_series(1, 1000 * 1000) t(x);
create index on t_dep (c);
analyze verbose t_dep;

-- thinks there are 5 * 10=50 distinct values
explain analyze select * from t_dep where b = 1 and c = 6;

create statistics st_dep (dependencies)
on a, b, c from t_dep;

analyze verbose t_dep;

execute check_stats \gx

-- thinks there are 10 distinct values
explain analyze select * from t_dep where b = 1 and c = 6;


\echo 'n distinct'
select x a, x % 5 as b, x % 10 as c
into table t_ndis
from generate_series(1, 1000 * 1000) t(x);
create index on t_dep (c);
analyze verbose t_ndis;

-- thinks there are 50 distinct values
explain analyze select b, c from t_ndis group by b, c;

create statistics st_ndis (ndistinct)
on b, c from t_ndis;

analyze verbose t_ndis;

-- thinks there are 10 distinct groups
explain select b, c from t_ndis group by b, c;

execute  analyze check_stats \gx

set enable_parallel_append=default;
set enable_async_append=default;
\set SINGLESTEP off
