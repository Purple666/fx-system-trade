#!/bin/bash -x
git add . && git commit -m "Experiment" && git push
oc login -u andesm -p a
oc get pods | egrep '(fx-system-trade.*build.*Running|fx-system-trade.*build.*Init:[0-9]|fx-system-trade.*build.*PodInitializing)'
./scaledown.sh
oc get pod | grep 'fx-system-trade-.*-build' | awk '{print $1}' | xargs oc delete pod
oc start-build fx-system-trade
oc get pods | grep 'fx-system-trade.*Terminating'
while [ $? = 0 ]
do
    sleep 5
    oc get pods | grep 'fx-system-trade.*Terminating'
done
while [ $? = 0 ]
do
    sleep 5
    oc get pods | egrep '(fx-system-trade.*build.*Running|fx-system-trade.*build.*Init:[0-9]|fx-system-trade.*build.*PodInitializing)'
done
oc get pods | egrep 'fx-system-trade.*build.*Error'
if  [ $? = 0 ] ; then
    exit
fi
sleep 10
python3 get_rate_data/clear_fx-trade.py
./scaleup.sh
