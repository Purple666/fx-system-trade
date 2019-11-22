#!/bin/bash -x

stack clean && stack install && stack exec -- fx-exe statistics


