from mytypes import *
from myhash import hash_password, verify_password
import exceptions as exs
from orm import Model


class Member(Model):
    def __init__(self, id: int, password: str, is_leader=False):
        self.id = id
        self.password = hash_password(password)
        self.is_leader = is_leader

    def is_frozen(self) -> bool:
        raise NotImplementedError

    @classmethod
    def list(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().list(**kwargs)

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        kwargs['password'] = hash_password(str(kwargs['password']))
        return super().create(**kwargs)

    @staticmethod
    def auth(member_id: int, password: str) -> Any:
        yield Member.get(id=member_id)
        member_password = password  # TODO
        if not verify_password(member_password, password):
            raise exs.IncorrectCredentials

    @classmethod
    def auth_as_leader(cls, member_id: int, password: str) -> Any:
        yield from cls.auth(member_id, password)
        if False:  # TODO member.is_leader
            raise exs.Forbidden


class Project(Model):
    def __init__(self, project_id: int, timestamp: int,
                 authority: int) -> None:
        self.id = project_id
        self.timestamp = timestamp
        self.authority = authority

    @classmethod
    def list(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().list(**kwargs)

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @staticmethod
    def get_list(*args, **kwargs) -> SQLQuery:
        raise NotImplementedError


class Action(Model):
    def __init__(self, id, timestamp: int, atype: str,
                 project_id: int, member_id: int) -> None:
        self.id = id
        self.timestamp = timestamp
        self.atype = atype
        self.project_id = project_id
        self.member_id = member_id

    @classmethod
    def list(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().list(**kwargs)

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @staticmethod
    def get_list(*args, **kwargs) -> SQLQuery:
        raise NotImplementedError


class Vote(Model):
    def __init__(self, timestamp: int, vtype: str,
                 member_id: int, action_id: int) -> None:
        self.timestamp = timestamp
        self.vtype = vtype
        self.member_id = member_id
        self.action_id = action_id

    @classmethod
    def list(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().list(**kwargs)

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)

    @staticmethod
    def get_members_votes(*args, **kwargs) -> SQLQuery:
        raise NotImplementedError


class Query(Model):
    def __init__(self, member_id, timestamp):
        self.member_id = member_id
        self.timestamp = timestamp

    @classmethod
    def list(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().list(**kwargs)

    @classmethod
    def get(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get(**kwargs)

    @classmethod
    def get_or_create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().get_or_create(**kwargs)

    @classmethod
    def create(cls, **kwargs: QueryParam) -> SQLQuery:
        return super().create(**kwargs)
