from toolz.curried import map, filter
from toolz.functoolz import curry, compose as C
from itertools import chain
from functools import partial as P

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
    def get_conds(cls, _conds: List[str]=None, **fields: QueryParam) -> str:
        conds = C(
            " AND ".join,
            P(chain, _conds or []),
            map('='.join),
            map(lambda k: (k, cls._val_holder(k))),
            filter(lambda k: fields[k] is not None),
            fields.keys
        )()
        return f" WHERE {conds}" if conds else ""

    @classmethod
    def get_ordering(cls, _order_by: QueryFields=None):
        fields = ", ".join(_order_by) if _order_by is not None else ""
        return f" ORDER BY {fields}" if fields else ""

    @classmethod
    def list(cls, _fields: QueryFields=None,
             _order_by: QueryFields=None,
             _conds: List[str]=None,
             **kwargs: QueryParam) -> SQLQuery:
        kwargs = {k: v for k, v in kwargs.items() if v is not None}
        table = cls.table_name()
        selected = ", ".join(_fields) if _fields is not None else "*"
        query = (f"SELECT {selected} FROM {table}"
                 f"{cls.get_conds(_conds, **kwargs)}"
                 f"{cls.get_ordering(_order_by)};")
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

    @staticmethod
    def count(field: str, val: str, name: str) -> str:
        return (
            f"COUNT(case {field} when '{val}' then 1 else null end) as {name}"
        )

    @classmethod
    def update(cls, field: str, val_name: str, value,
               selector: QueryParamsDict) -> SQLQuery:
        conds = cls.get_conds([], **selector)
        val_holder = cls._val_holder(val_name)
        q = f"UPDATE {cls.table_name()} SET {field}={val_holder}{conds};"
        return SQLQuery(q, {**selector, val_name: value})  # type: ignore
