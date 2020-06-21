#!/bin/bash -x

stack clean && stack build && stack exec -- fx-exe trade-sim



