from typing import Optional

from effect import Effect
from effect.do import do

from models import *
from exceptions import *


def transaction(f: Callable) -> Callable:
    def wrapper(*args, **kwargs):
        yield Effect(SQLQuery("BEGIN;"))
        yield from f(*args, **kwargs)
        yield Effect(SQLQuery("COMMIT;"))
    return wrapper


# Connection & Leader

@do
def open_conn(database: str, login: str, password: str) -> OpenDatabaseGen:
    yield Effect(OpenDatabase(database, login, password))


@do
def leader(password: str, member: TMember, timestamp: int) -> SQLQueryGen:
    yield Effect(Member.create(id=member, password=password,
                               is_leader=True, last_active=timestamp))


# Actions


@do
@transaction
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
    yield from Member.custom_get_or_create(member, password, timestamp)
    yield from Project.custom_get_or_create(project, timestamp, authority)
    yield Effect(Action.create(
        id=action, timestamp=timestamp, atype=action_type,
        project_id=project, member_id=member))
    yield Effect(UserActionVote.create(member_id=member, action_id=action))


def support(**kwargs) -> SQLQueryGen:
    return _action(SUPPORT, **kwargs)


def protest(**kwargs) -> SQLQueryGen:
    return _action(PROTEST, **kwargs)


@do
@transaction
def _vote(
        vote_type: str,
        timestamp: TTime,
        member: TMember,
        password: str,
        action: TAction,
    ) -> SQLQueryGen:
    assert(vote_type in [VOTE_UP, VOTE_DOWN])
    yield from Member.custom_get_or_create(member, password, timestamp)
    action_f = yield Effect(Action.get(id=action, _fields=['member_id']))
    actions = action_f()  # type: ignore
    if len(actions) == 0:
        raise exs.DoesNotExist
    action_member_id = actions[0][0]
    yield Effect(Vote.create(timestamp=timestamp, vtype=vote_type,
                             member_id=member, action_id=action))
    yield Effect(UserActionVote.add_vote(action_member_id, action, vote_type))


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
        type: Optional[TAType] = None,
        project: Optional[TProject] = None,
        authority: Optional[TAuthority] = None,
    ) -> SQLQueryGen:
    yield from Member.auth_as_leader(member, password, timestamp)
    yield Effect(Action.get_list(
        atype=type,
        project_id=project,
        authority=authority,
    ))


@do
def projects(
        timestamp: TTime,
        member: TMember,
        password: str,
        authority: Optional[TAuthority] = None,
    ) -> SQLQueryGen:
    yield from Member.auth_as_leader(member, password, timestamp)
    yield Effect(Project.get_list(authority=authority))


@do
def votes(
        timestamp: TTime,
        member: TMember,
        password: str,
        action: Optional[TAction] = None,
        project: Optional[TProject] = None,
    ) -> SQLQueryGen:
    yield from Member.auth_as_leader(member, password, timestamp)
    params = {'v.action_id': action, 'a.project_id': project}
    yield Effect(Vote.get_members_votes(**params))


@do
def trolls(timestamp: int) -> SQLQueryGen:
    yield Effect(UserActionVote.get_list())
