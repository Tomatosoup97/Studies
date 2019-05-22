from typing import Optional

from effect import Effect
from effect.do import do

from models import *
from exceptions import *


def user_is_frozen() -> bool:
    return False  # TODO: implement


# Connection & Leader

@do
def open_conn(database: str, login: str, password: str) -> OpenDatabaseGen:
    yield Effect(OpenDatabase(database, login, password))


@do
def leader(password: str, member: TMember) -> SQLQueryGen:
    yield Effect(Member.create(id=member, password=password, is_leader=True))


# Actions

@do
def _action(
        action_type: str,
        timestamp: TTime,
        member: TMember,
        password: str,
        action: TAction,
        project: TProject,
        authority: Optional[TAuthority] = None,
    ) -> SQLQueryGen:
    assert(action_type in [SUPPORT, PROTEST])
    user = yield Effect(Member.get_or_create(id=member, password=password))
    if user_is_frozen():
        raise UserIsFrozenError
    try:
        yield Effect(Project.get(id=project))
    except DoesNotExist:
        if authority is None:
            raise InvalidInputError
        yield Effect(Project.create(project=project, authority=authority,
                                    timestamp=timestamp))

    yield Effect(Action.create(
        id=action, timestamp=timestamp, atype=action_type,
        project_id=project, member_id=member))


def support(**kwargs) -> SQLQueryGen:
    return _action(SUPPORT, **kwargs)


def protest(**kwargs) -> SQLQueryGen:
    return _action(PROTEST, **kwargs)


@do
def _vote(
        vote_type: str,
        timestamp: TTime,
        member: TMember,
        password: str,
        action: TAction,
    ) -> SQLQueryGen:
    assert(vote_type in [VOTE_UP, VOTE_DOWN])
    yield Effect(Member.get_or_create(id=member, password=password))
    if user_is_frozen():
        raise UserIsFrozenError
    yield Effect(Action.get(id=action))
    yield Effect(Vote.create(timestamp=timestamp, vtype=vote_type,
                             member_id=member, action_id=action))


def upvote(**kwargs) -> SQLQueryGen:
    return _vote(VOTE_UP, **kwargs)


def downvote(**kwargs) -> SQLQueryGen:
    return _vote(VOTE_DOWN, **kwargs)


# Queries

@do
def actions(
        timestamp: TTime,
        member: TMember,
        password: str,
        atype: Optional[TAType] = None,
        project: Optional[TProject] = None,
        authority: Optional[TAuthority] = None,
    ) -> SQLQueryGen:
    yield from Member.auth_as_leader(member, password)
    yield Effect(Query.create(timestamp=timestamp, member_id=member))
    yield Effect(Action.get_list(
        atype=atype,
        project_id=project,
        authority=authority,
        order_by="id",
    ))


@do
def projects(
        timestamp: TTime,
        member: TMember,
        password: str,
        authority: Optional[TAuthority] = None,
    ) -> SQLQueryGen:
    yield from Member.auth_as_leader(member, password)
    yield Effect(Query.create(timestamp=timestamp, member_id=member))
    yield Effect(Project.get_list(
        authority=authority,
        order_by="id",
    ))


@do
def votes(
        timestamp: TTime,
        member: TMember,
        password: str,
        action: Optional[TAction] = None,
        project: Optional[TProject] = None,
    ) -> SQLQueryGen:
    yield from Member.auth_as_leader(member, password)
    yield Effect(Query.create(timestamp=timestamp, member_id=member))
    yield Effect(Vote.get_members_votes(
        action_id=action,
        project_id=project,
        order_by="member_id",
    ))


def trolls() -> SQLQueryGen:
    # TODO: Might need to be done on db-level
    pass
