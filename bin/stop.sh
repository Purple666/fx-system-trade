#!/bin/bash -x

cd docker

sudo docker-compose stop backtest trade
sudo docker-compose rm -f backtest trade
