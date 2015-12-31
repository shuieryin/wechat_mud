#!/bin/bash

app_name=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)
log_html_folder=$(ls -td -- $(ls -td -- ./_build/test/logs/*/ | head -n 1)lib.${app_name}.logs/* | head -n 1)

for filename in ${log_html_folder}/${app_name}_suite.*_*.html; do
    is_test_passed=$(cat ${filename} | grep -E "=== Returned value: ok|=== successfully completed test case")
    if [ "${is_test_passed}" = "" ]
    then
        lynx --dump ${filename} > tmp_result.txt
        cat tmp_result.txt
    fi
done

rm -f tmp_result.txt