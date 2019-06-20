from mytypes import SQLQuery
from orm import Model, join_queries, append_to_query


def test_joining_queries():
    q1 = SQLQuery("SELECT * FROM t1;", {'x': 'y'})
    q2 = SQLQuery("SELECT * FROM t2;", {'z': 'a'})
    res = join_queries(q2)(q1)
    assert res.q == "SELECT * FROM t1; SELECT * FROM t2;"
    assert res.params == {'x': 'y', 'z': 'a'}


def test_append_to_query():
    q = SQLQuery("SELECT * FROM t;", {'x': 'y'})
    res = append_to_query("WHERE %(x)s='y'")(q)
    assert res.q == "SELECT * FROM t WHERE %(x)s='y';"
    assert res.params == {'x': 'y'}


class TestORM:
    def test_table_name(self):
        assert Model.table_name() == "models"

    def test_list__zero_params(self):
        res = Model.list()
        assert res.q == "SELECT * FROM models;"
        assert res.params == {}

    def test_list__one_params(self):
        params = {'x': 'y'}
        res = Model.list(**params)
        assert res.q == "SELECT * FROM models WHERE x=%(x)s;"
        assert res.params == params

    def test_list__three_params(self):
        params = {'x': 'y', 'z': 1, 'f': 'g'}
        res = Model.list(**params)
        assert res.q == (
            "SELECT * FROM models WHERE x=%(x)s AND z=%(z)s AND f=%(f)s;")
        assert res.params == params

    def test_list__fields(self):
        params = {'x': 'y'}
        res = Model.list(_fields=('id', 'name'), **params)
        assert res.q == "SELECT id, name FROM models WHERE x=%(x)s;"
        assert res.params == params

    def test_list__ordering(self):
        params = {'x': 'y'}
        res = Model.list(_order_by=('id', 'name'), **params)
        assert res.q == "SELECT * FROM models WHERE x=%(x)s ORDER BY id, name;"
        assert res.params == params

    def test_get(self):
        params = {'x': 'y'}
        res = Model.get(**params)
        assert res.q == "SELECT * FROM models WHERE x=%(x)s LIMIT 1;"
        assert res.params == params

    def test_get__fields(self):
        params = {'x': 'y'}
        res = Model.get(_fields=("fst", "snd"), **params)
        assert res.q == "SELECT fst, snd FROM models WHERE x=%(x)s LIMIT 1;"
        assert res.params == params

    def test_create(self):
        params = {'x': 'y', 'z': 1}
        res = Model.create(**params)
        assert res.q == "INSERT INTO models (x, z) VALUES (%(x)s, %(z)s);"
        assert res.params == params

    def test_get_or_create(self):
        params = {'x': 'y', 'z': 1}
        res = Model.get_or_create(**params)
        assert res.q == (
            "INSERT INTO models (x, z) VALUES (%(x)s, %(z)s) ON CONFLICT DO "
            "NOTHING; SELECT * FROM models WHERE x=%(x)s AND z=%(z)s LIMIT 1;")
        assert res.params == params
