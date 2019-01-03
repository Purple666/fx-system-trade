#!/bin/bash -x

stack clean && stack build --trace && stack exec -- fx-exe backtest +RTS -xc


