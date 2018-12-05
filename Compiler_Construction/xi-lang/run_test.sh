#!/bin/sh
make
TEST_NAME="$1"
echo "----------  out  ----------" && \
./xi --plugin mods/mod_student.cma $TEST_NAME && \
echo "----------  log  -----------" && \
cat "xilog/005.translate.log" && \
echo "----------  ir   ----------" && \
cat "xilog/006.translate.translated.ir" && \
echo "----------  test ----------" && \
cat -n $TEST_NAME && \
echo "" && \
echo "----------  run  ----------" && \
spim -file main.s

