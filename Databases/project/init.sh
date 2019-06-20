DIR=tests/scenarios

./recreate_db.sh
echo "Running init_2..."
python main.py --init < $DIR/init_2
echo "Running run_2..."
python main.py < $DIR/run_2
echo "Done"
