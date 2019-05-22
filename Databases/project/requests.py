import json

from toolz.functoolz import compose as C, curry
from mytypes import *


def req_action(request: RequestType) -> str:
    assert len(request.keys()) == 1, "Invalid request format"
    return list(request.keys())[0]


@curry
def validate_req(is_init: bool, request: RequestType) -> RequestType:
    action = req_action(request)
    if is_init:
        assert action in ["leader"], "Init requests should define leaders"
    else:
        assert action not in ["leader", "open"], "Invalid action for request"
    return request


def read_request() -> RequestType:
    return C(RequestType, json.loads, input)(">")


def output_response(response: ResponseType) -> None:
    C(print, json.dumps)(response)


@curry
def validate_req_action(action: str, request: RequestType) -> RequestType:
    assert req_action(request) == action, f"Expected request type: {action}"
    return request
