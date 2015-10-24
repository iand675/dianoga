#!/usr/bin/env bash

set -ex

if [[ "$(uname)" == 'Darwin' ]]; then

  DYLD_LIBRARY_PATH=/usr/local/opt/icu4c/lib

  stack build --extra-include-dirs=/usr/local/opt/icu4c/include --extra-lib-dirs=/usr/local/opt/icu4c/lib

fi

