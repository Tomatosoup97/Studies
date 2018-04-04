#!/bin/bash
make -C src && docker-compose build && docker-compose up -d --force-recreate

