#!/bin/bash

DIR=tests/scenarios
mkdir -p logs

# ./recreate_db.sh
# python main.py --init < $DIR/init_1 > logs/actual_init_1
# python main.py < $DIR/run_1 > logs/actual_run_1

./recreate_db.sh
python main.py --init < $DIR/init_2 > logs/actual_init_2
python main.py < $DIR/run_2 > logs/actual_run_2

# diff $DIR/expected_init_1  logs/actual_init_1
# diff $DIR/expected_run_1  logs/actual_run_1

diff $DIR/expected_init_2  logs/actual_init_2
diff $DIR/expected_run_2  logs/actual_run_2

./recreate_db.sh
