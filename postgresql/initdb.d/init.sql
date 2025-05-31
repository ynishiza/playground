-- Step: user
CREATE ROLE yuinishizawa
SUPERUSER
LOGIN;

CREATE ROLE tester
SUPERUSER
LOGIN;

-- Step: create tap
CREATE SCHEMA tap;
SET search_path=tap;
CREATE EXTENSION pgtap;

-- Step: user settings
ALTER ROLE yuinishizawa IN DATABASE playground
  SET search_path=tap,public;
ALTER ROLE tester IN DATABASE playground
  SET search_path=tap,public;
GRANT ALL ON DATABASE playground TO yuinishizawa;
GRANT ALL ON DATABASE playground TO tester;

