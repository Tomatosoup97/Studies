#!/bin/sh
make
TEST_NAME="$1"
echo "----------  out  ----------"
./xi --plugin mods/mod_student.cma $TEST_NAME --stop-after typechecker
echo "---------- test  ----------"
cat -n $TEST_NAME
echo "---------- xilog ----------"
cat "xilog/003.typechecking.log"
echo "---------- types ----------"
cat "xilog/004.typechecking.types"

