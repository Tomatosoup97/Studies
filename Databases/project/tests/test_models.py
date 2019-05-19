from models import Member


class TestMember():
    def test_table_name(self):
        assert Member.table_name() == "members"

    def test_list(self):
        params = {'x': 'y', 'z': 1}
        res = Member.list(**params)
        assert res.q == "SELECT * FROM members WHERE x=%(x)s AND z=%(z)s;"
        assert res.params == params
