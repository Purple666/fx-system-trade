#!/bin/bash -x

oc scale deploymentconfig --replicas=1 fx-system-trade-backtest
#oc scale deploymentconfig --replicas=1 fx-system-trade-trade-sim
oc scale deploymentconfig --replicas=1 fx-system-trade-trade-practice
oc scale deploymentconfig --replicas=1 get-rate
