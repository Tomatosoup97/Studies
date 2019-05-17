from typing import List, Tuple, Optional

from models import *
from exceptions import *


# Connection & Leader

def open_conn(database: str, login: str, password: str) -> None:
    pass


def leader(password: str, member: TMember) -> None:
    Member.create(password, member, is_leader=True)


# Actions

def _action(
        action_type: str,
        timestamp: TTime,
        member: TMember,
        password: str,
        action: TAction,
        project: TProject,
        authority: Optional[TAuthority]
    ) -> None:
    assert(action_type in [SUPPORT, PROTEST])
    user = Member.get_or_create(id=member, password=password)
    if user.is_frozen():
        raise UserIsFrozenError
    try:
        Project.get(id=project)
    except DoesNotExist:
        if authority is None:
            raise InvalidInputError
        Project.create(project, authority=authority, timestamp=timestamp)
    Action.create(action, timestamp, action_type, project, member)


def support(*args, **kwargs) -> None:
    return _action(SUPPORT, *args, **kwargs)


def protest(*args, **kwargs) -> None:
    return _action(PROTEST, *args, **kwargs)


def _vote(
        vote_type: str,
        timestamp: TTime,
        member: TMember,
        password: str,
        action: TAction,
    ) -> None:
    assert(vote_type in [VOTE_UP, VOTE_DOWN])
    user = Member.get_or_create(id=member, password=password)
    if user.is_frozen():
        raise UserIsFrozenError
    Action.get(id=action)
    Vote.create(timestamp, vote_type, member, action)


def upvote(*args, **kwargs) -> None:
    return _vote(VOTE_UP, *args, **kwargs)


def downvote(*args, **kwargs) -> None:
    return _vote(VOTE_DOWN, *args, **kwargs)


# Queries

def actions(
        timestamp: TTime,
        member: TMember,
        password: str,
        atype: Optional[TAType],
        project: Optional[TProject],
        authority: Optional[TAuthority],
    ) -> List[Tuple[TProject, TAuthority]]:
    Member.auth_as_leader(member, password)
    Query.create(timestamp, member)
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
    ) -> List[Tuple[TProject, TAuthority]]:
    Member.auth_as_leader(member, password)
    Query.create(timestamp, member)
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
    ) -> List[Tuple[TMember, TUpvote, TDownvote]]:
    Member.auth_as_leader(member, password)
    Query.create(timestamp, member)
    return Vote.get_members_votes(
        action_id=action,
        project_id=project,
        order_by="member_id",
    )


def trolls() -> List[Tuple[TMember, TUpvote, TDownvote, TActive]]:
    # TODO: Might need to be done on db-level
    pass
