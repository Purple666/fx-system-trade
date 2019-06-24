#!/bin/bash -x
git add . && git commit -m "Experiment" && git push
oc login -u andesm -p a
./scaledown.sh
oc start-build fx-system-trade
oc get pods | grep 'fx-system-trade.*Terminating'
while [ $? = 0 ]
do
    sleep 5
    oc get pods | grep 'fx-system-trade.*Terminating'
done
oc get pods | egrep '(fx-system-trade.*build.*Running|fx-system-trade.*build.*Init:[0-9])'
while [ $? = 0 ]
do
    sleep 5
    oc get pods | egrep '(fx-system-trade.*build.*Running|fx-system-trade.*build.*Init:[0-9])'
    
done
sleep 10
python3 get_rate_data/clear_fx-trade.py
./scaleup.sh
