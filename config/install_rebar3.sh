#!/bin/bash

if [ -f "./config/rebar3" ]; then
    exit
fi

git clone --depth=1 https://github.com/rebar/rebar3.git
cd rebar3
./bootstrap
cd ..
cp ./rebar3/rebar3 ./config/
rm -rf rebar3