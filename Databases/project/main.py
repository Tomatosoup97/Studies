import sys
import typing as t

import psycopg2
from toolz.functoolz import compose as C
from effect import (sync_perform, sync_performer,
                    TypeDispatcher, ComposedDispatcher, base_dispatcher)

import api
import exceptions as exs
from requests import *


DEBUG = False
db_conn = None
db_cursor = None


@sync_performer
def perform_sql_query(dispatcher, sqlquery: SQLQuery):
    if DEBUG:
        print(sqlquery)
    assert db_cursor is not None, "Database is not opened, could not execute"
    db_cursor.execute(sqlquery.q, sqlquery.params)
    return db_cursor.fetchall


@sync_performer
def perform_open_db(dispatcher, intent: OpenDatabase) -> None:
    global db_conn, db_cursor
    if DEBUG:
        print(intent)
    db_conn = psycopg2.connect(dbname=intent.db, user=intent.login,
                               password=intent.password)
    db_cursor = db_conn.cursor()


def process_request(request: RequestType) -> ResponseType:
    api_dispatcher: t.Dict[str, t.Callable[..., ApiGenType]] = {
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
    action = req_action(request)
    effect = api_dispatcher[action](**request[action])
    eff_dispatcher = ComposedDispatcher([
        TypeDispatcher({
            SQLQuery: perform_sql_query,
            OpenDatabase: perform_open_db,
        }),
        base_dispatcher,
    ])
    sync_perform(eff_dispatcher, effect)
    assert db_cursor is not None and db_conn is not None, "DB conn closed"
    db_conn.commit()

    if db_cursor.rowcount != -1:
        data = db_cursor.fetchall()
        return ResponseType({
            "status": "OK",
            "data": list(map(list, data)),
        })
    return ResponseType({"status": "OK"})


def close_db_conn() -> None:
    assert db_cursor is not None and db_conn is not None, "DB already closed"
    db_cursor.close()
    db_conn.close()


@curry
def get_response(validate: Callable[[RequestType], RequestType],
                 request: RequestType) -> ResponseType:
    try:
        return C(process_request, validate)(request)
    except (exs.InternalException, psycopg2.Error) as e:
        return ResponseType({"status": "ERROR", "debug": str(e)})


def run(is_init=False) -> None:
    C(output_response,
      get_response(validate_req_action("open")),
      read_request)()

    while True:
        try:
            C(output_response,
              get_response(validate_req(is_init)),
              read_request)()
        except EOFError:
            close_db_conn()
            break


def init_db():
    # TODO
    pass


if __name__ == "__main__":
    if len(sys.argv) == 2 and sys.argv[1] == "--init":
        init_db()
        run(is_init=True)
    elif len(sys.argv) == 1:
        run(is_init=False)
    else:
        print("Invalid execution. Supported params: --init")
        sys.exit(1)
