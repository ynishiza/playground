
select
  nspname as schema,
  relname as name,
  relkind,
  attname as column_name,
  typname as type
  -- con.name
from
  pg_class
inner join pg_namespace on pg_class.relnamespace = pg_namespace.oid
inner join pg_attribute on pg_class.oid = pg_attribute.attrelid
inner join pg_type tp on pg_attribute.atttypid = tp.oid
where
  nspname = 'public'
;
