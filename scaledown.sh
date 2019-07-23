#!/bin/bash -x

oc scale deploymentconfig --replicas=0 fx-system-trade-backtest
oc scale deploymentconfig --replicas=0 fx-system-trade-trade-practice
oc scale deploymentconfig --replicas=0 get-rate
