#!/bin/bash -x

stack clean && stack build --profile --trace && stack exec -- fx-exe debug



