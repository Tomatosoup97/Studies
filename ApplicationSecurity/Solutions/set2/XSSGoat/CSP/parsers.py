from rest_framework import renderers
from rest_framework.parsers import JSONParser


class ReportParser(JSONParser):
    """
    Parses CSP Report in JSON-serialized data.
    """
    media_type = 'application/csp-report'
    renderer_class = renderers.JSONRenderer
