#!/bin/bash

cd _build/default/rel/wechat_mud/
redis-server &
./bin/wechat_mud console
