#!/bin/bash -x
git add . && git commit -m "Experiment" && git push
oc login -u andesm -p a
./scaledown.sh
oc start-build fx-system-trade
sleep 10
oc get pods | grep 'fx-system-trade.*Terminating'
while [ $? = 0 ]
do
    sleep 5
    oc get pods | grep 'fx-system-trade.*Terminating'
done
oc get pods | grep 'fx-system-trade.*build.*Running'
while [ $? = 0 ]
do
    sleep 5
    oc get pods | grep 'fx-system-trade.*build.*Running'
done
python get_rate_data/clear_fx-trade.py
./scaleup.sh
sleep 10
./log_trade-backtest.sh
