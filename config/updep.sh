#!/bin/bash

LIB_NAME=$1

if [[ -z "${LIB_NAME}" || "${LIB_NAME}" == " " ]]; then
        echo "no library name specified!"
        exit 1
fi

PATTERN="{<<\"${LIB_NAME}\">>,"
LINE_NUM=$(grep -nw rebar.lock -e ${PATTERN} | grep -o -E "^[0-9]+")

if [[ -z "${LINE_NUM}" || "${LINE_NUM}" == " " ]]; then
        echo "library [${LIB_NAME}] does not exist!"
        exit 1
fi

START_LINE_NUM=$(expr ${LINE_NUM} + 1)
END_LINE_NUM=$(expr ${LINE_NUM} + 3)

sed -i -e "${START_LINE_NUM},${END_LINE_NUM}d" rebar.lock
sed -i -e "s/${PATTERN}//g" rebar.lock

rm -rf _build/default/lib/${LIB_NAME}
make compile

echo "done!"