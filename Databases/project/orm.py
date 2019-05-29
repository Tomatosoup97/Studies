from toolz.curried import map
from toolz.functoolz import curry, compose as C

from mytypes import *


@curry
def join_queries(q2: SQLQuery, q1: SQLQuery) -> SQLQuery:
    return SQLQuery(f'{q1.q} {q2.q}', {**q1.params, **q2.params}, q2.fields)


@curry
def append_to_query(s: str, q: SQLQuery) -> SQLQuery:
    return SQLQuery(q.q.replace(';', f' {s};'), q.params, q.fields)


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
    def list(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        kwargs = {k: v for k, v in kwargs.items() if v is not None}
        table = cls.table_name()
        conds = C(
            " AND ".join,
            map('='.join),
            map(lambda k: (k, cls._val_holder(k))),
            kwargs.keys
        )()
        selected = ", ".join(_fields) if _fields is not None else "*"
        conds = f" WHERE {conds}" if conds else ""
        query = f"SELECT {selected} FROM {table}{conds};"
        return SQLQuery(query, kwargs, _fields)

    @classmethod
    def get(cls, _fields: QueryFields=None, **kwargs: QueryParam) -> SQLQuery:
        return C(
            append_to_query("LIMIT 1"),
            lambda k: cls.list(_fields, **k),
        )(kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        columns = cls._columns(**kwargs)
        vals = cls._vals_holders(**kwargs)
        return SQLQuery(f"INSERT INTO {cls.table_name()} ({columns}) "
                        f"VALUES ({vals});", kwargs)

    @classmethod
    def get_or_create(cls, _fields: QueryFields=None,
                      **kwargs: QueryParam) -> SQLQuery:
        return C(
            join_queries(cls.get(_fields, **kwargs)),
            append_to_query("ON CONFLICT DO NOTHING"),
            lambda: cls.create(**kwargs),
        )()
