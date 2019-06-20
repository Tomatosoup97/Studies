from typing import *

SUPPORT = "support"
PROTEST = "protest"
VOTE_UP = "up"
VOTE_DOWN = "down"

TTime = int
TMember = int
TUpvote = int
TDownvote = int
TAction = int
TProject = int
TActive = bool
TAuthority = int
TAType = str

TLogin = str
LoginValues = ["app", "init"]

ValueType = Union[str, int]

RequestType = NewType(
    'RequestType',
    Dict[str, Dict[str, ValueType]],
)
ResponseType = NewType(
    'ResponseType',
    Dict[str, Union[str, List[Any]]],
)

QueryParam = Union[str, int]
QueryParamsDict = Dict[str, QueryParam]
QueryFields = Optional[List[str]]


class SQLQuery:
    def __init__(self,
                 q: str,
                 params: Optional[Dict[str, QueryParam]]=None,
                 fields: QueryFields=None) -> None:
        self.q = q
        self.params = params if params is not None else {}
        self.fields = fields

    def __str__(self) -> str:
        return f"SQLQuery<{self.q}, {str(self.params)}>"


class OpenDatabase:
    def __init__(self, db: str, login: str, password: str) -> None:
        self.db = db
        self.login = login
        self.password = password

    def __str__(self) -> str:
        return f"OpenDatabase<db={self.db}, user={self.login}>"


class CurrentDatetime:
    pass


SQLQueryGen = Iterable[SQLQuery]
OpenDatabaseGen = Iterable[SQLQuery]
ApiGenType = Union[SQLQueryGen, OpenDatabaseGen]
