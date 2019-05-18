import json
import typing as t

import api
from mytypes import *
from common import compose as C


def process_request(request: RequestType) -> ResponseType:
    dispatcher: t.Dict[str, t.Callable[..., t.Optional[Any]]] = {
        "open": api.open_conn,
        "leader": api.leader,
        # Actions
        "support": api.support,
        "protest": api.support,
        "upvote": api.upvote,
        "downvote": api.downvote,
        # Queries
        "actions": api.actions,
        "projects": api.projects,
        "votes": api.votes,
        "trolls": api.trolls,
    }
    assert(len(request.keys()) == 1)
    action = list(request.keys())[0]
    data = dispatcher[action](**request[action])
    if data is not None:
        return ResponseType({
            "status": "OK",
            "data": list(map(list, data)),
        })
    return ResponseType({"status": "OK"})


if __name__ == "__main__":
    # TODO: close db connection
    while True:
        request = C(RequestType, json.loads, input)(">")
        response = process_request(request)
        C(print, json.dumps)(response)
