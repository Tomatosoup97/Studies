from toolz.curried import map
from toolz.functoolz import curry

from mytypes import *
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
