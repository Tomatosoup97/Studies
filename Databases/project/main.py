import json
import typing as t
from toolz.functoolz import compose as C
from effect import (sync_perform, sync_performer,
                    TypeDispatcher, ComposedDispatcher, base_dispatcher)

import api
from mytypes import *


@sync_performer
def perform_sql_query(dispatcher, sqlquery: SQLQuery):
    print(sqlquery)


def process_request(request: RequestType) -> ResponseType:
    api_dispatcher: t.Dict[str, t.Callable[..., t.Optional[Any]]] = {
        "open": api.open_conn,
        "leader": api.leader,
        # Actions
        "support": api.support,
        "protest": api.protest,
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

    effect = api_dispatcher[action](**request[action])
    eff_dispatcher = ComposedDispatcher([
        TypeDispatcher({SQLQuery: perform_sql_query}),
        base_dispatcher,
    ])
    sync_perform(eff_dispatcher, effect)
    data = None

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
