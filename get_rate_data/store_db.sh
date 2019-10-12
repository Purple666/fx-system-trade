#!/bin/bash -x

echo 'ticker,date,time,open,high,low,close' > header.txt

cat org/*.txt | grep USDJPY | sort >  /tmp/fx.csv
cat header.txt /tmp/fx.csv | python3 csv2db.py 

rm /tmp/fx.csv header.txt




