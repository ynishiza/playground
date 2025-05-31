-- Step: user
CREATE ROLE tester
SUPERUSER
LOGIN;

-- Step: install
CREATE SCHEMA tap;
SET search_path=tap;
CREATE EXTENSION pgtap WITH SCHEMA tap;

-- Step: user settings
ALTER ROLE tester IN DATABASE playground
  SET search_path=tap,public;
GRANT ALL ON DATABASE playground TO tester;
