#!/bin/bash

COMPILE_ROUTER="make -C src"
RUN_ROUTER="./src/router < $1/$2"

docker exec -it \
    $(docker ps -a | grep "router_$2" | awk '{ print $1}') \
    sh -c "${COMPILE_ROUTER} && echo && ${RUN_ROUTER}"

