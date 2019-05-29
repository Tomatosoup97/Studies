#!/bin/bash
mypy .
flake8 .
pytest .
#./run_tests.sh
