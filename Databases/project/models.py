from collections import namedtuple

from mytypes import *
from myhash import hash_password, verify_password
import exceptions as exs
from orm import Model
from effect import Effect


MemberData = namedtuple('MemberData', ['password', 'is_leader'])


class Member(Model):
    def __init__(self, id: int, password: str, is_leader=False):
        self.id = id
        self.password = hash_password(password)
        self.is_leader = is_leader

    @staticmethod
    def is_frozen() -> bool:
        # TODO: implement
        return False

    @classmethod
    def list(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().list(_fields, **kwargs)

    @classmethod
    def get(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().get(_fields, **kwargs)

    @classmethod
    def get_or_create(cls, _fields: QueryFields=None,
                      **kwargs: QueryParam) -> SQLQuery:
        kwargs['password'] = hash_password(str(kwargs['password']))
        return super().get_or_create(_fields, **kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        kwargs['password'] = hash_password(str(kwargs['password']))
        return super().create(**kwargs)

    @staticmethod
    def auth(member_id: int, password: str) -> Any:
        # TODO: Might want to have separation between models and effects
        fields = ["password", "is_leader"]
        member_f = yield Effect(Member.get(_fields=fields, id=member_id))
        member = MemberData(*member_f()[0])
        if not verify_password(member.password, password):
            raise exs.IncorrectCredentials
        return member

    @classmethod
    def auth_as_leader(cls, member_id: int, password: str) -> Any:
        member = yield from cls.auth(member_id, password)
        if not member.is_leader:
            raise exs.Forbidden

    @classmethod
    def custom_get_or_create(cls, member: int, password: str) -> SQLQueryGen:
        user_f = yield Effect(Member.get(id=member))
        if len(user_f()) == 0:
            yield Effect(Member.create(id=member, password=password))
        elif cls.is_frozen():
            raise exs.UserIsFrozenError
        else:
            yield from Member.auth(member, password)


class Project(Model):
    def __init__(self, project_id: int, timestamp: int,
                 authority: int) -> None:
        self.id = project_id
        self.timestamp = timestamp
        self.authority = authority

    @classmethod
    def list(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().list(_fields, **kwargs)

    @classmethod
    def get(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().get(_fields, **kwargs)

    @classmethod
    def get_or_create(cls, _fields: QueryFields=None,
                      **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(_fields, **kwargs)

    @staticmethod
    def custom_get_or_create(project: int, timestamp: int,
                             authority=None) -> SQLQueryGen:
        project_f = yield Effect(Project.get(id=project))
        if len(project_f()) == 0:
            if authority is None:
                raise exs.InvalidInputError
            yield Effect(Project.create(id=project, authority=authority,
                                        timestamp=timestamp))

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @classmethod
    def get_list(cls, **kwargs) -> SQLQuery:
        # TODO
        _fields = ['id', 'authority']
        return cls.list(_fields, **kwargs)


class Action(Model):
    def __init__(self, id, timestamp: int, atype: str,
                 project_id: int, member_id: int) -> None:
        self.id = id
        self.timestamp = timestamp
        self.atype = atype
        self.project_id = project_id
        self.member_id = member_id

    @classmethod
    def list(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().list(_fields, **kwargs)

    @classmethod
    def get(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().get(_fields, **kwargs)

    @classmethod
    def get_or_create(cls, _fields: QueryFields=None,
                      **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(_fields, **kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @classmethod
    def get_list(cls, *args, **kwargs) -> SQLQuery:
        groupby_fields = ['a.id', 'atype', 'project_id', 'authority']
        result_fields = ', '.join(groupby_fields + [
            cls.count('v.vtype', 'up', 'upvotes'),
            cls.count('v.vtype', 'down', 'downvotes'),
        ])
        q = (f"SELECT {result_fields} FROM actions as a "
             f"JOIN projects p ON(p.id=project_id) "
             f"JOIN votes v ON (a.id=v.action_id)"
             f"{cls.get_conds(**kwargs)} "
             f"GROUP BY {', '.join(groupby_fields)} ORDER BY a.id;")
        return SQLQuery(q, kwargs)


class Vote(Model):
    def __init__(self, timestamp: int, vtype: str,
                 member_id: int, action_id: int) -> None:
        self.timestamp = timestamp
        self.vtype = vtype
        self.member_id = member_id
        self.action_id = action_id

    @classmethod
    def list(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().list(_fields, **kwargs)

    @classmethod
    def get(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().get(_fields, **kwargs)

    @classmethod
    def get_or_create(cls, _fields: QueryFields=None,
                      **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(_fields, **kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @classmethod
    def get_members_votes(cls, **kwargs) -> SQLQuery:
        fields = ', '.join([
            'members.id',
            cls.count('v.vtype', 'up', 'upvotes'),
            cls.count('v.vtype', 'down', 'downvotes'),
        ])
        q = (f"SELECT {fields} FROM members "
             f"JOIN votes v ON (v.member_id=members.id) "
             f"JOIN actions a ON (a.id=v.action_id)"
             f"{cls.get_conds(**kwargs)} "
             f"GROUP BY members.id ORDER BY members.id;")
        return SQLQuery(q, kwargs)


class Query(Model):
    def __init__(self, member_id, timestamp):
        self.member_id = member_id
        self.timestamp = timestamp

    @classmethod
    def table_name(cls) -> str:
        return 'queries'

    @classmethod
    def list(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().list(_fields, **kwargs)

    @classmethod
    def get(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return super().get(_fields, **kwargs)

    @classmethod
    def get_or_create(cls, _fields: QueryFields=None,
                      **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(_fields, **kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)
