#!/bin/bash -x

cd docker

sudo docker-compose stop backtest backtest-retry
sudo docker-compose rm -f backtest backtest-retry
