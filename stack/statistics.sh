#!/bin/bash -x

stack clean && stack install --profile && stack exec --profile -- fx-exe statistics +RTS -xc


