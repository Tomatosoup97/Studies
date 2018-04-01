#!/bin/bash

COMPILE_ROUTER="make -C src/router"
RUN_ROUTER="./src/router < ${TEST_SUITE}/$1"

docker exec -it \
    $(docker ps -a | grep "router_$1" | awk '{ print $1}') \
    sh -c "${COMPILE_ROUTER} && ${RUN_ROUTER}"

