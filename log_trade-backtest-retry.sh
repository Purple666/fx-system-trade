#!/bin/bash

oc login -u andesm -p a
while [ 1 ]
do
    sleep 5
    oc get pods | grep 'fx-system-trade-backtest-retry.*' | awk '{ print $1}' | xargs oc logs -f
done
