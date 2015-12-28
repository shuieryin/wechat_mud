#!/bin/bash

echo "is on travis:[" ${TRAVIS_CI} "]"
if [  "true" != "$TRAVIS_CI" ]
then
    exit
fi

log_folder=$(ls -td -- ./_build/test/logs/*/ | head -n 1)
create_time_raw=${log_folder##*@}
create_time=${create_time_raw//\/}
file_name=test_log_${create_time}.zip

zip -r ${file_name} ${log_folder} > /dev/null

from="shuieryin@localhost.localdomain"
to="shuieryin@gmail.com"
subject="Travis CI build failed - ${create_time}"
body="Path: ${log_folder}"
attachments="${file_name}"

./config/sendmail.sh "${from}" "${to}" "${subject}" "${body}" "${attachments}"
rm -f ${file_name}