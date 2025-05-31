-- \if off
--
create or replace function af() returns text as 'begin return (select 1); end;' language plpgsql
;

create or replace function tryCall(query text) returns text as $body$
declare
  result text;
  err_state text;
  err_msg text;

begin
  execute query into result;
  return result;
exception
when others then
  get stacked diagnostics
    err_state := returned_sqlstate
    err_msg := message_tex
    ;
  return format('%s %s', err_state, err_msg);
end;
$body$ language plpgsql;

