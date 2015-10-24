#!/bin/bash

APP_NAME=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)
OLD_VSN=$(cat rebar.config | grep release-version-marker | awk '{print $1}' | tr -d \")

NEW_VSN=$(./config/appup ${APP_NAME} ${OLD_VSN} | tr -d \")

if [ "${NEW_VSN}" = "no_change" ]; then
    echo "No changed files"
else
    rebar3 release relup -u ${OLD_VSN} tar
    yes | cp -i _build/default/rel/${APP_NAME}/releases/${NEW_VSN}/${APP_NAME}.rel _build/default/rel/${APP_NAME}/releases/

    mv _build/default/rel/${APP_NAME}/${APP_NAME}-${NEW_VSN}.tar.gz _build/default/rel/${APP_NAME}/releases/${NEW_VSN}/wechat_mud.tar.gz
    ./_build/default/rel/${APP_NAME}/bin/${APP_NAME} install ${NEW_VSN}
fi
