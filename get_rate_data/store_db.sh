#!/bin/bash -x

echo 'ticker,date,time,open,high,low,close' > header.txt

for file in org/*.txt
do
    base_file=$(basename -s .txt $file)
    if [ ! -e comp/${base_file}_comp.txt ]
    then 
        grep USDJPY $file > tmp.txt
        cat header.txt tmp.txt | python comp.py > comp/${base_file}_comp.txt
    fi 
done    

cat comp/*_comp.txt | sort >  fx.csv

echo 'date,bid' > header.txt
cat header.txt fx.csv | python csv2db.py 

rm fx.csv header.txt tmp.txt




