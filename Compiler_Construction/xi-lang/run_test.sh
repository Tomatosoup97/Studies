#!/bin/sh
TEST_NAME="$1"
make && \
echo "----------  uwr  ----------" && \
./xi --extra-debug $TEST_NAME > /dev/null 2>&1 && \
rm uwr_xilog -rf && \
cp -r xilog uwr_xilog && \
echo "----------  out  ----------" && \
./xi --extra-debug --plugin mods/mod_student.cma $TEST_NAME && \
echo "----------  test ----------" && \
cat -n $TEST_NAME && \
echo "" && \
echo "----------  run  ----------" && \
spim -file main.s

