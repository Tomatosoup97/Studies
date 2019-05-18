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

SQLQuery = NewType(
    'SQLQuery',
    Tuple[str, Dict[str, QueryParam]],
)
