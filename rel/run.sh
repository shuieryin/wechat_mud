#!/bin/bash

cd /root/workspaces/wechat_mud/rel/wechat_mud_latest/
redis-server &
./bin/wechat_mud console

