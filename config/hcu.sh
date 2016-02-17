#!/bin/bash

## Check the codes whether pass compilation, print errors if not, and perform release upgrade otherwise.
echo -n "Compiling..."
COMPILE_RESULT=$(rebar3 compile)
IS_COMPILE_SUCCESS=$(echo ${COMPILE_RESULT} | awk '{print $8}')
if [ "${IS_COMPILE_SUCCESS}" = "Compiling" ]; then
    ## the compilation failed result does not switch line so it is for breaking lines for pretty print the errors.
    echo ${COMPILE_RESULT} | sed -E s/\=\=\=\>/\\n\=\=\=\>/g | sed -E s/failed/failed\\n/g
else
    echo "done"

    ## retrieve app name, old version and from rebar.config, and new version by performing ./config/appup.es.
    APP_NAME=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)
    OLD_VSN=$(cat rebar.config | grep release-version-marker | awk '{print $1}' | tr -d \")
    NEW_VSN=$(./config/appup.es gen_appup ${APP_NAME} ${OLD_VSN} | tr -d \")

    ## perform release upgrade only if changes exists.
    if [ "${NEW_VSN}" = "no_change" ]; then
        echo "No added/deleted/modified files"
    elif [[ "$NEW_VSN" == *"error"* ]]; then
        echo "${NEW_VSN}"
    else
        ## copy, remove, move the src folder is because the modifies that synchronized from intellij does not
        ## work for release upgrade and cause remains unknown, this is the work around since no other solution is found.
        cp -rf src src_bak
        rm -rf src
        mv src_bak src

        ## execute release upgrade command, version number after "-u" is the upgrade-from version.
        rebar3 release relup -u ${OLD_VSN} tar

        ## copy [APP_NAME].rel from the latest release folder to parent releases folder because rebar3 only perform
        ## this action once which will cause the auto release upgrade only works for two time, there we do this manually here.
        rm -f _build/default/rel/${APP_NAME}/releases/${APP_NAME}.rel
        cp _build/default/rel/${APP_NAME}/releases/${NEW_VSN}/${APP_NAME}.rel _build/default/rel/${APP_NAME}/releases/

        ## rebar3 does not put the release upgrade gzip package under the latest release version folder so do it manually.
        mv _build/default/rel/${APP_NAME}/${APP_NAME}-${NEW_VSN}.tar.gz _build/default/rel/${APP_NAME}/releases/${NEW_VSN}/wechat_mud.tar.gz
        ./_build/default/rel/${APP_NAME}/bin/${APP_NAME} install ${NEW_VSN}
    fi
fi