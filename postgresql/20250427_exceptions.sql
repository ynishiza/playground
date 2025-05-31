CREATE OR REPLACE PROCEDURE testerror() AS $$
DECLARE
  msg_state text;
  msg_message text;
  msg_detail text;
  msg_hint text;
  msg_context text;
  msg_schema text;
  msg_table text;
  msg_column text;
  msg_constraint text;
  msg_datatype text;
BEGIN
  -- RAISE plpgsql_error USING
  RAISE SQLSTATE 'YUI00' USING
    MESSAGE='oops',
    DETAIL='some internal error',
    HINT='no hint',
    SCHEMA='myschema',
    TABLE='mytable',
    COLUMN='col1',
    DATATYPE='text',
    CONSTRAINT='CHECK'
    ;

  EXCEPTION
    -- WHEN plpgsql_error THEN
    WHEN SQLSTATE 'YUI00' THEN
      GET STACKED DIAGNOSTICS
        msg_state := RETURNED_SQLSTATE,
        msg_message := MESSAGE_TEXT,
        msg_detail := PG_EXCEPTION_DETAIL,
        msg_hint := PG_EXCEPTION_HINT,
        msg_context := PG_EXCEPTION_CONTEXT,
        msg_table := TABLE_NAME,
        msg_schema := SCHEMA_NAME,
        msg_column := COLUMN_NAME,
        msg_constraint := CONSTRAINT_NAME,
        msg_datatype := PG_DATATYPE_NAME
        ;

      RAISE INFO '
state=%
message=%
context=%
detail=%
hint=%
schema=%
table=%
column=%
constraint=%
data type=%
',
        msg_state,
        msg_message,
        msg_context,
        msg_detail,
        msg_hint,
        msg_schema,
        msg_table,
        msg_column,
        msg_constraint,
        msg_datatype
        ;
END;
$$ LANGUAGE PLPGSQL;

call testerror();
