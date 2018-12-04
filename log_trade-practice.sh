#!/bin/bash

oc get pods | grep 'fx-system-trade-trade-practice.*Running' | awk '{ print $1}' | xargs oc logs -f
