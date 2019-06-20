import datetime
from collections import namedtuple

from mytypes import *
from myhash import hash_password, verify_password
import exceptions as exs
from orm import Model
from effect import Effect

LA_THRESHOLD = 31 * 6  # 6 months
MemberData = namedtuple('MemberData', ['password', 'is_leader',
                                       'last_active', 'is_active'])


class Member(Model):
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
    def is_frozen(member: MemberData) -> Any:
        current_date = yield Effect(CurrentDatetime())  # type: ignore
        threshold = int((
            current_date - datetime.timedelta(days=LA_THRESHOLD)
        ).timestamp())
        return member.last_active < threshold

    @classmethod
    def set_last_active(cls, member_id: int, timestamp: int) -> SQLQuery:
        return cls.update("last_active", "timestamp",
                          timestamp, {'id': member_id})

    @classmethod
    def set_is_active(cls, member_id: int, is_active: bool) -> Any:
        is_active_v = str(is_active).lower()
        params = ["is_active", "is_active_v", is_active_v, {'id': member_id}]
        yield Effect(cls.update(*params))  # type: ignore
        yield Effect(UserActionVote.update(*params))  # type: ignore

    @classmethod
    def auth(cls, member_id: int, password: str, timestamp: int) -> Any:
        fields = ["password", "is_leader", "last_active", "is_active"]
        member_f = yield Effect(Member.get(_fields=fields, id=member_id))
        member = MemberData(*member_f()[0])
        is_frozen = yield from cls.is_frozen(member)
        if not verify_password(member.password, password):
            raise exs.IncorrectCredentials
        if is_frozen:
            raise exs.UserIsFrozenError
        if is_frozen != member.is_active:
            yield from cls.set_is_active(member_id, not is_frozen)
        yield Effect(cls.set_last_active(member_id, timestamp))
        return member

    @classmethod
    def auth_as_leader(cls, member_id: int, password: str,
                       timestamp: int) -> Any:
        member = yield from cls.auth(member_id, password, timestamp)
        if not member.is_leader:
            raise exs.Forbidden

    @classmethod
    def custom_get_or_create(
            cls, member: int, password: str, timestamp: int,
        ) -> SQLQueryGen:
        user_f = yield Effect(Member.get(id=member))
        if len(user_f()) == 0:  # type: ignore
            yield Effect(Member.create(id=member, password=password,
                                       last_active=timestamp))
        else:
            yield from Member.auth(member, password, timestamp)


class Project(Model):
    @staticmethod
    def custom_get_or_create(project: int, timestamp: int,
                             authority=None) -> SQLQueryGen:
        project_f = yield Effect(Project.get(id=project))
        if len(project_f()) == 0:  # type: ignore
            if authority is None:
                raise exs.InvalidInputError
            yield Effect(Project.create(id=project, authority=authority,
                                        timestamp=timestamp))

    @classmethod
    def get_list(cls, **kwargs) -> SQLQuery:
        _fields = ['id', 'authority']
        return cls.list(_fields, **kwargs)


class Action(Model):
    @classmethod
    def get_list(cls, *args, **kwargs) -> SQLQuery:
        groupby_fields = ['a.id', 'atype', 'project_id', 'authority']
        result_fields = ', '.join(groupby_fields + [
            cls.count('v.vtype', 'up', 'upvotes'),
            cls.count('v.vtype', 'down', 'downvotes'),
        ])
        q = (f"SELECT {result_fields} FROM actions as a "
             f"JOIN projects p ON(p.id=project_id) "
             f"FULL OUTER JOIN votes v ON (a.id=v.action_id)"
             f"{cls.get_conds(**kwargs)} "
             f"GROUP BY {', '.join(groupby_fields)} ORDER BY a.id;")
        return SQLQuery(q, kwargs)


class Vote(Model):
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


class UserActionVote(Model):
    @classmethod
    def table_name(cls) -> str:
        return 'user_actions_votes'

    @classmethod
    def get_list(cls, **kwargs) -> SQLQuery:
        _fields = ['member_id', 'upvotes', 'downvotes', 'is_active']
        _order_by = ['(downvotes - upvotes) DESC', 'member_id']
        _conds = ["(downvotes - upvotes) > 0"]
        return cls.list(_fields, _order_by, _conds, **kwargs)

    @classmethod
    def add_vote(cls, member_id: int,
                 action_id: int, vote_type: str) -> SQLQuery:
        assert(vote_type in [VOTE_UP, VOTE_DOWN])
        col = f"{vote_type}votes"
        params = {'member_id': member_id, 'action_id': action_id}
        conds = cls.get_conds(**params)
        return SQLQuery(  # type: ignore
            f"UPDATE {cls.table_name()} SET {col}={col}+1 {conds};", params)
