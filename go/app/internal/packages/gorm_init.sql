-- run this with
--
--  psql -f internal/packages/gorm_init.sql -U yuinishizawa postgres
--
DROP DATABASE IF EXISTS gormtest;
DROP ROLE IF EXISTS gormtest;
CREATE ROLE gormtest WITH LOGIN;
CREATE DATABASE gormtest WITH OWNER gormtest;
