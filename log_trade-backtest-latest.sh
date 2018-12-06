#!/bin/bash

oc get pods | grep 'fx-system-trade-backtest-latest.*Running' | awk '{ print $1}' | xargs oc logs -f
