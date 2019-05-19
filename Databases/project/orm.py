from toolz.curried import map
from toolz.functoolz import curry, compose as C

from mytypes import *


def _join_queries(q2: SQLQuery, q1: SQLQuery) -> SQLQuery:
    return SQLQuery(f'{q1.q} {q2.q}', {**q1.params, **q2.params})


def _append_to_query(s: str, query: SQLQuery) -> SQLQuery:
    return SQLQuery(query.q.replace(';', f' {s};'), query.params)


append_to_query = curry(_append_to_query)
join_queries = curry(_join_queries)


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
    def list(cls, **kwargs: QueryParam) -> SQLQuery:
        conds = C(
            " AND ".join,
            map('='.join),
            map(lambda k: (k, cls._val_holder(k))),
            kwargs.keys
        )()
        conds = f" WHERE {conds}" if conds else ""
        return SQLQuery(f"SELECT * FROM {cls.table_name()}{conds};", kwargs)

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return C(
            append_to_query("LIMIT 1"),
            lambda k: cls.list(**k),
        )(kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        columns = cls._columns(**kwargs)
        vals = cls._vals_holders(**kwargs)
        return SQLQuery(f"INSERT INTO {cls.table_name()} ({columns}) "
                        f"VALUES ({vals});", kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return C(
            join_queries(cls.get(**kwargs)),
            append_to_query("ON CONFLICT DO NOTHING"),
            lambda k: cls.create(**k),
        )(kwargs)
