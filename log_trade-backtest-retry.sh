#!/bin/bash

oc get pods | grep 'fx-system-trade-backtest-retry.*' | awk '{ print $1}' | xargs oc logs -f
