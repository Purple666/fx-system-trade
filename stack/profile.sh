#!/bin/bash -x

stack clean && stack build --profile && stack exec -- fx-exe test +RTS -p


