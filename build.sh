#!/bin/bash -x

cd docker

docker-compose stop backtest
docker-compose rm -f backtest
docker rmi -f $(docker images | awk '/^<none>/ { print $3 }')
python ../get_rate_data/clear_fx-trade.py
docker-compose build && docker-compose up -d
