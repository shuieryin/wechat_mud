#!/bin/bash

cd /root/workspaces/wechat_mud/_build/default/rel/wechat_mud/
redis-server &
./bin/wechat_mud console
