rollback;

\set inputpath './jp1/data_trimmed.csv'
begin;

create or replace function cleantext(text, text default ' ') returns text as $$
  select regexp_replace($1, E'[\\n\\r]+', $2, 'g');
$$ language sql;

drop table if exists jp1table cascade;
drop type if exists jobtype cascade;

create type jobtype as enum ('job', 'net');


-- step 1: load data
create table  jp1table (
  --ユニット名
  unit_name text not null,
  -- 上位ユニット完全名
  unit_path text not null,
  -- ユニット種別
  unit_type char not null,
 -- 位置情報
  x int,
  y int,
	-- サイズ
  width numeric,
  height numeric,
	-- コメント
  comment text,
	-- 先行
  unit_ancestors text,
	-- 接続種別
  unit_ancestor_types text,
  -- 実行エージェント
  agent text,

  -- 実行順序制御
  jikkou_jyunjo text,
	-- 多重起動
  tajyuu_kidou text,
	-- 保存世代数
  hozon_sedai int,
	-- 優先順位
  yuusen_juni int,
	-- 上位のジョブネットに依存する
  jyoui_izon char,
	-- 有効範囲
  yuukou text,
	-- ジョブグループ名
  jobgroup text,
	-- 排他ジョブネット名
  haita_jobnet text,
	-- リンクするルール番号
  rule int,
	-- 開始日
  start_time text,
	-- 処理サイクル
  cycle text,
	-- 遅延監視
  chien_start text,
  chien_end text,
	-- 起動条件
  kidou text,
	-- コマンド
  command text
);

\prompt "File name e.g. data_trimmed.csv\t" source_file

select
  case trim(:'source_file')
      when '' then 'data_trimmed.csv'
      else trim(:'source_file')
  end
as source_file \gset

\echo 'Loading ' || :source_file

select format($$
  copy jp1table
  from PROGRAM 'tail -n +16 /Users/yuinishizawa/Projects/playground/postgresql/jp1/%s'
  with (
    format 'csv',
    header false
  );
  $$,
  :'source_file'
  -- case trim(:'source_file')
  --     when '' then 'data_trimmed.csv'
  --     else trim(:'source_file')
  --   end
)\gexec
-- copy jp1table
-- from PROGRAM 'tail -n +16 /Users/yuinishizawa/Projects/playground/postgresql/jp1/data_trimmed.csv'
-- with (
--   format 'csv',
--   header false
-- );


-- step 2: cleanup
alter table jp1table
  alter column unit_ancestors set data type text[]
    using string_to_array(cleantext(unit_ancestors, '|'), '|'),
  alter column unit_type set data type jobtype
    using cast(case unit_type when 'n' then 'net' else 'job' end as jobtype),
  alter column unit_path set data type text[]
    using string_to_array(trim(both '/' from unit_path), '/'),

  --
  add column unit_level int,
  add column unit_parent text not null generated always AS (
    unit_path[cardinality(unit_path)]
  ) stored,
  add column batch_process_target text,
  add column outbound_id text
  ;

select min(cardinality(unit_path)) base_length from jp1table \gset
\echo Base length :base_length

update jp1table
  set
    unit_ancestor_types = cleantext(unit_ancestor_types, '|'),
    unit_level = cardinality(unit_path) - :base_length,
    batch_process_target = (regexp_match(command, 'TARGET="([\S]+)"'))[1],
    outbound_id = (regexp_match(command, 'OUTBOUND_ID=([0-9a-zA-Z]+)'))[1]
  ;

alter table jp1table
  alter column unit_level set not null;


drop view if exists jp1_edges;

create view jp1_edges AS (
      select
        unit_name,
        unit_path,
        unit_type,
        unit_level,
        unit_parent,
        comment,
        unnest(unit_ancestors) as unit_source
      from jp1table
);

select * from jp1table limit 1 \gx

commit;
