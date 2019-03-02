#!/bin/bash

oc login -u andesm -p a
oc get pods | grep 'fx-system-trade-backtest-[0-9].*Running' | awk '{ print $1}' | xargs oc logs -f
