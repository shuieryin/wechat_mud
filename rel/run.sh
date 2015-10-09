#!/bin/bash

redis-server &
cd /root/workspaces/wechat_mud/rel/wechat_mud/
./bin/wechat_mud console
