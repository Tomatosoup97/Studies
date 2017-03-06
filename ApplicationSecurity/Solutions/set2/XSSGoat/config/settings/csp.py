from .base import *


DEBUG = True

# Content Security Policy
# ---------------------------------------------------------------------------

MIDDLEWARE += [
    'csp.middleware.CSPMiddleware',
]

CSP_DEFAULT_SRC = (
    "'self'",
)
CSP_STYLE_SRC = (
    "'self'",
    'fonts.googleapis.com',
    'yui.yahooapis.com',
    'yui-s.yahooapis.com',
)
CSP_FONT_SRC = (
    "'self'",
    'fonts.googleapis.com',
    'https://fonts.gstatic.com',
)
CSP_SCRIPT_SRC = (
    "'self'",
    'code.jquery.com',
    "'sha256-xlerleBaoEq8gwdfMhJE3oXSMlHpN6NX0lz1Pb1v3uM='",
)

CSP_EXCLUDE_URL_PREFIXES = (
    "/admin",
)

# CSP Report Only

CSP_REPORT_ONLY = True
CSP_REPORT_URI = 'http://127.0.0.1:8000/csp/api'
