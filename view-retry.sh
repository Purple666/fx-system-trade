#!/bin/bash -x

cd docker
while [ 1 ]
do
    docker-compose logs -f backtest-retry
done
