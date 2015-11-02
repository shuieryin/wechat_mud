#!/bin/bash

echo -n "Compiling..."
COMPILE_RESULT=$(rebar3 compile)
IS_COMPILE_SUCCESS=$(echo ${COMPILE_RESULT} | awk '{print $8}')
if [ "${IS_COMPILE_SUCCESS}" = "Compiling" ]; then
    echo ${COMPILE_RESULT} | sed -E s/\=\=\=\>/\\n\=\=\=\>/g | sed -E s/failed/failed\\n/g
else
    echo "done"
    APP_NAME=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)
    OLD_VSN=$(cat rebar.config | grep release-version-marker | awk '{print $1}' | tr -d \")
    NEW_VSN=$(./config/appup.es gen_appup ${APP_NAME} ${OLD_VSN} | tr -d \")
    if [ "${NEW_VSN}" = "no_change" ]; then
        echo "No added/deleted/modified files"
    else
        cp -rf src src_bak
        rm -rf src
        mv src_bak src
        rebar3 release relup -u ${OLD_VSN} tar
        rm -f _build/default/rel/${APP_NAME}/releases/${APP_NAME}.rel
        cp _build/default/rel/${APP_NAME}/releases/${NEW_VSN}/${APP_NAME}.rel _build/default/rel/${APP_NAME}/releases/

        mv _build/default/rel/${APP_NAME}/${APP_NAME}-${NEW_VSN}.tar.gz _build/default/rel/${APP_NAME}/releases/${NEW_VSN}/wechat_mud.tar.gz
        ./_build/default/rel/${APP_NAME}/bin/${APP_NAME} install ${NEW_VSN}
    fi
fi