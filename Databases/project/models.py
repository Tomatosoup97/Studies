from typing import List, Tuple
from mytypes import *
from myhash import hash_password, verify_password
import exceptions as exs


class Model:
    """Base class for ORM Model"""
    @staticmethod
    def get(*args, **kwargs):
        raise NotImplementedError

    @staticmethod
    def create(*args, **kwargs) -> None:
        raise NotImplementedError

    @staticmethod
    def get_or_create(*args, **kwargs):
        raise NotImplementedError

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

    @staticmethod
    def auth(member_id: int, password: str) -> Member:
        member = Member.get(id=member_id)
        if not verify_password(member.password, password):
            raise exs.IncorrectCredentials
        return member

    @classmethod
    def auth_as_leader(cls, member_id: int, password: str) -> Member:
        member = cls.auth(member_id, password)
        if not member.is_leader:
            raise exs.Forbidden
        return member


class Action(Model):
    def __init__(self, id, timestamp: int, atype: str,
                 project_id: int, member_id: int) -> None:
        self.id = id
        self.timestamp = timestamp
        self.atype = atype
        self.project_id = project_id
        self.member_id = member_id

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

    @staticmethod
    def get_members_votes(*args, **kwargs) \
            -> List[Tuple[TMember, TUpvote, TDownvote]]:
        raise NotImplementedError


class Project(Model):
    def __init__(self, project_id: int, timestamp: int,
                 authority: int) -> None:
        self.id = project_id
        self.timestamp = timestamp
        self.authority = authority

    @staticmethod
    def get_list(*args, **kwargs) -> List[Tuple[TProject, TAuthority]]:
        raise NotImplementedError


class Query(Model):
    def __init__(self, member_id, timestamp):
        self.member_id = member_id
        self.timestamp = timestamp
