#!/bin/bash -x

echo 'ticker,date,time,open,high,low,close' > header.txt

sort org/*-jpy.txt >  /tmp/fx.csv
#sort org/220906-jpy.txt >  /tmp/fx.csv
cat header.txt /tmp/fx.csv | python3 csv2db.py 

rm /tmp/fx.csv header.txt




