from models import Member, Project


class TestMember():
    def test_table_name(self):
        assert Member.table_name() == "members"

    def test_list(self):
        params = {'x': 'y', 'z': 1}
        res = Member.list(**params)
        assert res.q == "SELECT * FROM members WHERE x=%(x)s AND z=%(z)s;"
        assert res.params == params


class TestProject():
    def test_get_list(self):
        params = {'authority': 1}
        res = Project.get_list(**params)
        assert res.q == ("SELECT id, authority FROM projects "
                         "WHERE authority=%(authority)s;")
        assert res.params == params

    def test_get_list__none_param(self):
        params = {'authority': None}
        res = Project.get_list(**params)
        assert res.q == ("SELECT id, authority FROM projects;")
        assert res.params == {}
