#!/bin/bash

DIR=tests/scenarios
mkdir -p logs
python main.py --init < $DIR/init_1 > logs/actual_init_1
python main.py < $DIR/run_1 > logs/actual_run_1
# diff $DIR/expected_init_1  logs/actual_init_1
# diff $DIR/expected_run_1  logs/actual_run_1
