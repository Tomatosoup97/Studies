from typing import List, Tuple
from toolz.curried import map
from toolz.functoolz import curry

from mytypes import *
from myhash import hash_password, verify_password
import exceptions as exs
from common import compose as C


def _bind_queries(q2: SQLQuery, q1: SQLQuery) -> SQLQuery:
    return SQLQuery((f'{q1[0]} {q2[0]}', {**q1[1], **q2[1]}))


def _append_to_query(s: str, query: SQLQuery) -> SQLQuery:
    return SQLQuery((query[0].replace(';', f' {s};'), query[1]))


append_to_query = curry(_append_to_query)
bind_queries = curry(_bind_queries)


class Model:
    """Base class for ORM Model"""
    @staticmethod
    def _columns(**kwargs: QueryParam) -> str:
        return C(', '.join, kwargs.keys)()

    @staticmethod
    def _val_holder(key: str) -> str:
        return f'%({key})s'

    @classmethod
    def _vals_holders(cls, **kwargs) -> str:
        return C(', '.join, map(cls._val_holder), kwargs.keys)()

    @classmethod
    def table_name(cls) -> str:
        return f'{cls.__name__.lower()}s'

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        conds = C(
            " AND ".join,
            map('='.join),
            map(lambda k: (k, cls._val_holder(k))),
            kwargs.keys
        )()
        conds = f"WHERE {conds}" if conds else ""
        return SQLQuery(
            (f"SELECT * FROM {cls.table_name()} {conds} LIMIT 1;", kwargs))

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        columns = cls._columns(**kwargs)
        vals = cls._vals_holders(**kwargs)
        return SQLQuery((f"INSERT INTO {cls.table_name()} ({columns}) "
                         f"VALUES ({vals});", kwargs))

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return C(
            bind_queries(cls.get(**kwargs)),
            append_to_query("ON CONFLICT DO NOTHING"),
            lambda k: cls.create(**k),
        )(kwargs)

    @staticmethod
    def filter(*args, **kwargs):
        raise NotImplementedError

    def save(self, *args, **kwargs) -> None:
        raise NotImplementedError


class Member(Model):
    def __init__(self, id: int, password: str, is_leader=False):
        self.id = id
        self.password = hash_password(password)
        self.is_leader = is_leader

    def is_frozen(self) -> bool:
        raise NotImplementedError

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        kwargs['password'] = hash_password(str(kwargs['password']))
        return super().create(**kwargs)

    @staticmethod
    def auth(member_id: int, password: str) -> Any:
        member = Member.get(id=member_id)
        # TODO
        member_password = password
        if not verify_password(member_password, password):
            raise exs.IncorrectCredentials
        return member

    @classmethod
    def auth_as_leader(cls, member_id: int, password: str) -> Any:
        member = cls.auth(member_id, password)
        if not member.is_leader:
            raise exs.Forbidden
        return member


class Project(Model):
    def __init__(self, project_id: int, timestamp: int,
                 authority: int) -> None:
        self.id = project_id
        self.timestamp = timestamp
        self.authority = authority

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @staticmethod
    def get_list(*args, **kwargs) -> List[Tuple[TProject, TAuthority]]:
        raise NotImplementedError


class Action(Model):
    def __init__(self, id, timestamp: int, atype: str,
                 project_id: int, member_id: int) -> None:
        self.id = id
        self.timestamp = timestamp
        self.atype = atype
        self.project_id = project_id
        self.member_id = member_id

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @staticmethod
    def get_list(*args, **kwargs) -> List[Tuple[TProject, TAuthority]]:
        raise NotImplementedError


class Vote(Model):
    def __init__(self, timestamp: int, vtype: str,
                 member_id: int, action_id: int) -> None:
        self.timestamp = timestamp
        self.vtype = vtype
        self.member_id = member_id
        self.action_id = action_id

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @staticmethod
    def get_members_votes(*args, **kwargs) \
            -> List[Tuple[TMember, TUpvote, TDownvote]]:
        raise NotImplementedError


class Query(Model):
    def __init__(self, member_id, timestamp):
        self.member_id = member_id
        self.timestamp = timestamp

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)
