#!/bin/bash -x

cd src

for f in *.hs
do
    stylish-haskell -i ${f}
    hlint --refactor  --refactor-options="--inplace" ${f}
done    
