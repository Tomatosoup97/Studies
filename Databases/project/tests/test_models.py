from models import Member, Project, Vote, Action


class TestMember:
    def test_table_name(self):
        assert Member.table_name() == "members"

    def test_list(self):
        params = {'x': 'y', 'z': 1}
        res = Member.list(**params)
        assert res.q == "SELECT * FROM members WHERE x=%(x)s AND z=%(z)s;"
        assert res.params == params


class TestProject:
    def test_get(self):
        params = {'id': 1}
        res = Project.get(**params)
        assert res.q == ("SELECT * FROM projects "
                         "WHERE id=%(id)s LIMIT 1;")
        assert res.params == params

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

    def test_list(self):
        res = Project.list()
        assert res.q == ("SELECT * FROM projects;")
        assert res.params == {}


class TestVotes:
    BASE = ("SELECT members.id, "
            "COUNT(case v.vtype when 'up' then 1 else null end) as upvotes, "
            "COUNT(case v.vtype when 'down' then 1 else null end) as downvotes"
            " FROM members "
            "JOIN votes v ON (v.member_id=members.id) "
            "JOIN actions a ON (a.id=v.action_id) "
            "{}"
            "GROUP BY members.id ORDER BY members.id;")

    def test_get_members_votes(self):
        res = Vote.get_members_votes()
        assert res.q == self.BASE.format("")
        assert res.params == {}

    def test_get_members_votes__filter(self):
        params = {'votes.action_id': 10}
        res = Vote.get_members_votes(**params)
        assert res.q == self.BASE.format(
            "WHERE votes.action_id=%(votes.action_id)s ")
        assert res.params == {'votes.action_id': 10}


class TestAction:
    BASE = ("SELECT a.id, atype, project_id, authority, "
            "COUNT(case v.vtype when 'up' then 1 else null end) as upvotes, "
            "COUNT(case v.vtype when 'down' then 1 else null end) as downvotes"
            " FROM actions as a "
            "JOIN projects p ON(p.id=project_id) "
            "FULL OUTER JOIN votes v ON (a.id=v.action_id) "
            "{}"
            "GROUP BY a.id, atype, project_id, authority ORDER BY a.id;")

    def test_get_list(self):
        res = Action.get_list()
        assert res.q == self.BASE.format("")
        assert res.params == {}

    def test_get_list__filter(self):
        params = {'atype': 'support'}
        res = Action.get_list(**params)
        assert res.q == self.BASE.format("WHERE atype=%(atype)s ")
        assert res.params == params

    def test_get_list__filter_two_conds(self):
        params = {'atype': 'support', 'project_id': 20}
        res = Action.get_list(**params)
        assert res.q == self.BASE.format(
            "WHERE atype=%(atype)s AND project_id=%(project_id)s ")
        assert res.params == params
