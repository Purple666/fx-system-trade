#!/bin/bash -x
oc delete -f openshift-fx-system-trade.yml
oc create -f openshift-fx-system-trade.yml
