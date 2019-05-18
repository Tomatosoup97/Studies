import psycopg2
from typing import Optional

from models import *
from exceptions import *


def user_is_frozen() -> bool:
    return False  # TODO: implement


# Connection & Leader

def open_conn(database: str, login: str, password: str) -> None:
    conn = psycopg2.connect(dbname=database, user=login, password=password)
    return conn.cursor()


def leader(password: str, member: TMember) -> SQLQuery:
    return Member.create(id=member, password=password, is_leader=True)


# Actions

def _action(
        action_type: str,
        timestamp: TTime,
        member: TMember,
        password: str,
        action: TAction,
        project: TProject,
        authority: Optional[TAuthority]
    ) -> SQLQuery:
    assert(action_type in [SUPPORT, PROTEST])
    Member.get_or_create(id=member, password=password)
    if user_is_frozen():
        raise UserIsFrozenError
    try:
        Project.get(id=project)
    except DoesNotExist:
        if authority is None:
            raise InvalidInputError
        Project.create(project=project, authority=authority,
                       timestamp=timestamp)
    return Action.create(id=action, timestamp=timestamp, atype=action_type,
                         project_id=project, member_id=member)


def support(*args, **kwargs) -> SQLQuery:
    return _action(SUPPORT, *args, **kwargs)


def protest(*args, **kwargs) -> SQLQuery:
    return _action(PROTEST, *args, **kwargs)


def _vote(
        vote_type: str,
        timestamp: TTime,
        member: TMember,
        password: str,
        action: TAction,
    ) -> SQLQuery:
    assert(vote_type in [VOTE_UP, VOTE_DOWN])
    Member.get_or_create(id=member, password=password)
    if user_is_frozen():
        raise UserIsFrozenError
    Action.get(id=action)
    return Vote.create(timestamp=timestamp, vtype=vote_type,
                       member_id=member, action_id=action)


def upvote(*args, **kwargs) -> SQLQuery:
    return _vote(VOTE_UP, *args, **kwargs)


def downvote(*args, **kwargs) -> SQLQuery:
    return _vote(VOTE_DOWN, *args, **kwargs)


# Queries

def actions(
        timestamp: TTime,
        member: TMember,
        password: str,
        atype: Optional[TAType],
        project: Optional[TProject],
        authority: Optional[TAuthority],
    ) -> SQLQuery:
    Member.auth_as_leader(member, password)
    Query.create(timestamp=timestamp, member_id=member)
    return Action.get_list(
        atype=atype,
        project_id=project,
        authority=authority,
        order_by="id",
    )


def projects(
        timestamp: TTime,
        member: TMember,
        password: str,
        authority: Optional[TAuthority],
    ) -> SQLQuery:
    Member.auth_as_leader(member, password)
    Query.create(timestamp=timestamp, member_id=member)
    return Project.get_list(
        authority=authority,
        order_by="id",
    )


def votes(
        timestamp: TTime,
        member: TMember,
        password: str,
        action: Optional[TAction],
        project: Optional[TProject],
    ) -> SQLQuery:
    Member.auth_as_leader(member, password)
    Query.create(timestamp=timestamp, member_id=member)
    return Vote.get_members_votes(
        action_id=action,
        project_id=project,
        order_by="member_id",
    )


def trolls() -> SQLQuery:
    # TODO: Might need to be done on db-level
    pass
