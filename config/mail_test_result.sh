#!/bin/bash

log_folder=$(ls -td -- ./_build/test/logs/*/ | head -n 1)

if ["$TRAVIS_CI" != "true"]; then
    exit
fi

file_name=test_result.zip

zip -r ${file_name} ${log_folder} > /dev/null

from="shuieryin@localhost.localdomain"
to="shuieryin@gmail.com"
subject="Travis CI build failed - ${log_folder##*@}"
body="Travis CI build failed attachment\n${log_folder}"
attachments="${file_name}"

./config/sendmail.sh "${from}" "${to}" "${subject}" "${body}" "${attachments}"
rm -f ${file_name}
echo "is on travis:" $TRAVIS_CI
....