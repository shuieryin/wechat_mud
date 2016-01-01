#!/bin/bash

if [  "true" = "$TRAVIS_CI" ]
then
    exit
fi

lynx --dump ./_build/test/cover/index.html > tmp_result_output
cat tmp_result_output | grep %
rm -f tmp_result_output