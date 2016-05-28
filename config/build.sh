#!/bin/bash

rm -rf _build/default/lib/wechat_mud/
rm -rf _build/default/rel/wechat_mud/
rm -f ebin/wechat_mud.appup
./config/rebar3 release
