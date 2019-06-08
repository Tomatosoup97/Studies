#!/bin/bash

DIR=tests/scenarios
mkdir -p logs

./recreate_db.sh
echo "Running init_1..."
python main.py --init < $DIR/init_1 > logs/actual_init_1
diff $DIR/expected_init_1 logs/actual_init_1

echo "Running run_1..."
python main.py < $DIR/run_1 > logs/actual_run_1
diff $DIR/expected_run_1 logs/actual_run_1

./recreate_db.sh
echo "Running init_2..."
python main.py --init < $DIR/init_2 > logs/actual_init_2
diff $DIR/expected_init_2 logs/actual_init_2

echo "Running run_2..."
python main.py < $DIR/run_2 > logs/actual_run_2
diff $DIR/expected_run_2 logs/actual_run_2

./recreate_db.sh
echo "Running init_errors..."
python main.py --init < $DIR/init_errors > logs/actual_init_errors
diff $DIR/expected_init_errors logs/actual_init_errors

echo "Running run_errors..."
diff $DIR/expected_run_errors logs/actual_run_errors
python main.py < $DIR/run_errors > logs/actual_run_errors

./recreate_db.sh
