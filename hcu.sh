#!/bin/bash

CURRENT_TAG = $(git describe --abbrev=0 --tags)
CURRENT_VERSION = ${CURRENT_TAG} | sed -e "s/v//g"

rebar3 relflow -u ${CURRENT_TAG}
rebar3 release relup -u ${CURRENT_VERSION} tar

LATEST_VERSION = $(git describe --abbrev=0 --tags) | sed -e "s/v//g"
mv _build/default/rel/wechat_mud/wechat_mud-${LATEST_VERSION}.tar.gz _build/default/rel/wechat_mud/releases/${LATEST_VERSION}/wechat_mud.tar.gz
./_build/default/rel/wechat_mud/bin/wechat_mud install ${LATEST_VERSION}
