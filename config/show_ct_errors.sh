#!/bin/bash

app_name=$(cat rebar.config | grep app-name-marker | awk '{print $1}' | tr -d ,)
log_html_folder=$(ls -td -- $(ls -td -- ./_build/test/logs/*/ | head -n 1)lib.${app_name}.logs/* | head -n 1)
all_tests_passed=true

for filename in ${log_html_folder}/${app_name}_suite.*_*.html; do
    is_test_passed=$(cat ${filename} | grep -E "=== Returned value: ok|=== successfully completed test case")
    if [ "${is_test_passed}" = "" ]
    then
        ./config/html2text.py ${filename}
        all_tests_passed=false
    fi
done

if [ "${all_tests_passed}" = "true" ]
then
    echo All tests passed.
fi