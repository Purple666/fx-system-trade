#!/bin/bash -x

cd src

for f in *.hs
do
    stylish-haskell -i ${f}
    hlint --refactor ${f} > tmp.hs
    mv tmp.hs ${f}
done    
