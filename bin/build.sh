#!/bin/bash -x

cd docker

docker-compose build fx-mongo && docker-compose up -d fx-mongo 
docker-compose stop backtest trade
docker-compose rm -f backtest trade
docker rmi -f $(docker images | awk '/^<none>/ { print $3 }')
#python ../get_rate_data/clear_fx-trade.py
docker-compose build && docker-compose up -d

