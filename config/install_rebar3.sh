#!/bin/bash

if [ -f "./config/rebar3" ]; then
    exit
fi

export http_proxy=${LOCAL_PROXY}
export https_proxy=${LOCAL_PROXY}

git clone --depth=1 https://github.com/rebar/rebar3.git
cd rebar3
./bootstrap
cd ..
cp ./rebar3/rebar3 ./config/
rm -rf rebar3