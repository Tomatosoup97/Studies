-- Database tables definition

CREATE TABLE members (
    id integer PRIMARY KEY,
    password varchar(255) NOT NULL,
    is_leader boolean DEFAULT false
);

CREATE TABLE projects (
    id integer PRIMARY KEY,
    timestamp integer NOT NULL,
    authority int NOT NULL
);

CREATE TABLE actions (
    id integer PRIMARY KEY,
    timestamp integer NOT NULL,
    atype char(7) CHECK (atype IN ('support', 'protest')) NOT NULL,
    project_id integer REFERENCES projects (id) NOT NULL,
    member_id integer REFERENCES members (id) NOT NULL
);

CREATE TABLE votes (
    id serial PRIMARY KEY,
    timestamp integer NOT NULL,
    vtype varchar(4) CHECK (vtype IN ('up', 'down')) NOT NULL,
    action_id integer REFERENCES actions (id) NOT NULL,
    member_id integer REFERENCES members (id) NOT NULL,
    UNIQUE (action_id, member_id)
);

CREATE TABLE queries (
    timestamp integer NOT NULL,
    member_id integer REFERENCES members (id) NOT NULL,
    PRIMARY KEY (timestamp, member_id)
);