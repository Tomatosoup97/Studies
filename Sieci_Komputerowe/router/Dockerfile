FROM debian:jessie

ENV ROUTER_DIR /var/www/router

ADD . ${ROUTER_DIR}
WORKDIR ${ROUTER_DIR}

RUN apt-get -qq update && \
    apt-get -qq -y install \
        build-essential && \
    make -C ${ROUTER_DIR}/src

CMD ["sh", "-c", "echo .${ROUTER_DIR}/src/router < test1/${MACHINE_NAME}"]

