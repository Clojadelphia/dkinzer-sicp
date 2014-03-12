#!/bin/bash

set -e

source ./lib/colors.sh

for file in $(ls ./lib/*.scm ./src/*.scm)
do
  echo -e "$Yel Compiling $file $RCol"
  echo ""
  echo '(cf "'$file'")' | scheme --quiet --eval
done
