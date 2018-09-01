#!/bin/bash -x

cd src

for f in *.hs
do
    hlint --refactor ${f} > tmp.hs
    mv tmp.hs ${f}
    stylish-haskell -i ${f}
done    
