#!/bin/bash
mypy .
flake8 .
pytest .
