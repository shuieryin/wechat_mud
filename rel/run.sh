#!/bin/bash

cd /root/workspaces/wechat_mud/rel/wechat_mud/
redis-server &
./bin/wechat_mud console
