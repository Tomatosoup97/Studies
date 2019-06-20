DROP DATABASE dbproject_test;
CREATE DATABASE dbproject_test;
DROP ROLE IF EXISTS init;
CREATE ROLE init WITH encrypted password 'passwd' LOGIN CREATEROLE;
GRANT ALL PRIVILEGES ON DATABASE dbproject_test TO init;
