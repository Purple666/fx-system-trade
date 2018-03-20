#!/bin/bash -x

echo 'ticker,date,time,open,high,low,close' > header.txt

cat org/*.txt | grep USDJPY | sort >  fx.csv
cat header.txt fx.csv | python csv2db.py 

rm fx.csv header.txt




