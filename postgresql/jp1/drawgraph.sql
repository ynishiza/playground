\set toplevel 'unit_level = 1'

create or replace function draw_edge(e jp1_edges) returns text as $$
      select
        format(
          '"%s" -> "%s"[]',
          e.unit_source,
          e.unit_name,
          e.comment
        )
$$ language sql;

create or replace function draw_node(n jp1table) returns text as $$
      select
        format(
          '"%1$s" [ label="%1$s | %2$s | target:%4$s
          outbound:%5$s" xlabel="%1$s" xlp="50" color="%3$s" fillcolor="silver" %6$s]',
          -- '"%1$s" [ label="%1$s | %2$s | target:%4$s
          -- outbound:%5$s" xlabel="%1$s" xlp="50" color="%3$s" %6$s %7$s]',
          n.unit_name,
          n.comment,
          case n.unit_type
            when 'net' then 'orange'
            else 'cyan'
          end,
          n.batch_process_target,
          n.outbound_id,
          case n.unit_type
          when 'net' then 'URL="' || n.unit_name || '.svg" '
            else ''
          end
          -- case n.unit_type
          -- when 'net' then 'shapefile="jp1/out/' || n.unit_name || '.svg" '
          --   else ''
          -- end
        )
$$ language sql;

-- copy (
--     VALUES
--       ('digraph {'),
--       ('graph [ rankdir="LR" ranksep="2.0" overlap="false" splines="line"]'),
--       ('node [ nodesep="3.0" height="1" shape="record" style="filled,rounded" penwidth="5.0"]'),
--       ('edge [ ]')
--     union all
--     (select draw_edge(jp1_edges) from jp1_edges where :toplevel)
--     union all
--     (select draw_node(jp1table) from jp1table where :toplevel)
--     union all
--     values
--       ('}')
-- )
--   to '/Users/yuinishizawa/Projects/playground/postgresql/jp1/temp2.dot'
--   with (
--     format text
--   )
--   ;

-- create or replace function draw(parent text) returns void as $$
-- begin
--   raise notice 'hello';

    -- execute 'copy (select 1) to '/tmp/a/' with (format text);';
  -- execute format($$
    -- copy (
    --   VALUES
    --     ('digraph {'),
    --     ('graph [ rankdir="LR" ranksep="2.0" overlap="false" splines="line"]'),
    --     ('node [ nodesep="3.0" height="1" shape="record" style="filled,rounded" penwidth="5.0"]'),
    --     ('edge [ ]')
    --   union all
    --   (select draw_edge(jp1_edges) from jp1_edges where unit_parent= %s)
    --   union all
    --   (select draw_node(jp1table) from jp1table where unit_parent= %s)
    --   union all
    --   values
    --     ('}')
  -- )
    -- to '/Users/yuinishizawa/Projects/playground/postgresql/jp1/temp2.dot'
    -- with (
    --   format text
    -- )
    -- ;
  -- $$, parent);
-- end
-- $$ language plpgsql;

create or replace procedure draw(name text) as $$
begin
    execute format(
    '
    copy (
      VALUES
        (''digraph {''),
        (''graph [ rankdir="LR" ranksep="2.0" overlap="false" splines="line"]''),
        (''node [ nodesep="3.0" height="1" shape="record" style="rounded" penwidth="5.0"]''),
        (''edge [ ]'')
      union all
      (select draw_edge(jp1_edges) from jp1_edges where unit_parent= ''%1$s'')
      union all
      (select draw_node(jp1table) from jp1table where unit_parent= ''%1$s'')
      union all
      values
        (''}'')
  )
    to ''%2$s/%1$s.dot''
    with (
      format text
    )
    ',
   name, '/Users/yuinishizawa/Projects/playground/postgresql/jp1/out');
end
$$ language plpgsql;

create or replace procedure drawall() as $$
declare
  parent text;
begin
  for parent in
      select distinct unit_parent from jp1table
  loop
      execute format('call draw(''%s'')', parent);
  end loop;
end;
$$ language plpgsql;

call drawall();

call draw('SLP01N_00001');

-- -y invert
\! rm -f graph.svg && dot -Tsvg -y jp1/out/SLP01N_00001.dot  > jp1/out/SLP01N_00001.svg

call draw('SLP01N_OSLP2851X01');
-- \! rm -f graph.svg && dot -Tsvg -y jp1/out/SLP01N_OSLP2851X01.dot  > jp1/out/SLP01N_OSLP2851X01.svg
call draw('SLP01N_OSLP2852X01');
-- \! rm -f graph.svg && dot -Tsvg -y jp1/out/SLP01N_OSLP2852X01.dot  > jp1/out/SLP01N_OSLP2852X01.svg
\! bash jp1/graph.sh
