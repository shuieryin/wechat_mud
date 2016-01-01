#!/bin/bash

app_name=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)
version=$(cat rebar.config | grep release-version-marker | awk '{print $1}' | tr -d \")

mkdir -p doc
cp ./config/overview.edoc ./doc/

sed -i.bak "s/{app_name}/${app_name}/1" ./doc/overview.edoc
sed -i.bak "s/{version}/${version}/1" ./doc/overview.edoc
rm -f ./doc/overview.edoc.bak

./config/rebar3 edoc