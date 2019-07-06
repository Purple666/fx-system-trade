#!/bin/bash -x
#https://www.forexite.com/free_forex_quotes/2016/04/260416.zip

oc login -u andesm -p a
oc scale deploymentconfig --replicas=0 fx-system-trade-backtest-latest
oc scale deploymentconfig --replicas=0 fx-system-trade-backtest-retry
oc scale deploymentconfig --replicas=0 fx-system-trade-backtest
oc scale deploymentconfig --replicas=0 fx-system-trade-trade-practice

function command_error() {
    exit 1
}

cd org

for year in $(seq 2019 2019)
do
    for month in $(seq 1 12)
    do
	for day in $(seq 1 31)
	do
	    y=$(($year-2000))
	    y=$(printf "%02d" $y)
	    m=$(printf "%02d" $month)
	    d=$(printf "%02d" $day)
            now=$(date +%y%m%d)
            if [ $y$m$d -lt $now -a ! -e $d$m$y.txt ]
            then 
	        wget https://www.forexite.com/free_forex_quotes/$year/$m/$d$m$y.zip
	        unzip $d$m$y.zip
            fi
	done    
    done    
done    
    
rm *.zip

cd ..

./store_db.sh

oc scale deploymentconfig --replicas=1 fx-system-trade-backtest-latest
#oc scale deploymentconfig --replicas=1 fx-system-trade-backtest-retry
oc scale deploymentconfig --replicas=1 fx-system-trade-backtest
oc scale deploymentconfig --replicas=1 fx-system-trade-trade-practice




