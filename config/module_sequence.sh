#!/bin/bash

APP_NAME=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)

erlc ./config/module_sequence.erl
MOD_SEQ=$(erl -noshell +pc unicode -name module_sequnce@127.0.0.1 -setcookie ${APP_NAME} -s module_sequence exec -s init stop)
rm -f module_sequence.beam

# The last dot is needed for parsing result to term in erlang!!
echo "$MOD_SEQ".