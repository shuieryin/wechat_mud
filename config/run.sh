#!/bin/bash

cd /root/workspaces/wechat_mud/_build/default/rel/wechat_mud_release/
redis-server &
./bin/wechat_mud_release console
