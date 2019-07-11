#!/bin/bash -x
oc delete -f openshift-fx-system-trade.yml
./prune.sh
oc create -f openshift-fx-system-trade.yml
./scaledown.sh
#./scaleup.sh
