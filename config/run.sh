#!/bin/bash

APP_NAME=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)
redis-server &
./_build/default/rel/${APP_NAME}/bin/${APP_NAME} console