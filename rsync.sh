#!/bin/bash -x

rsync -avuL --delete --exclude=org --exclude=comp --exclude=.stack-work --exclude=.git . 192.168.1.62:fx-sytem-trade

