#!/bin/bash

set -e

source ./lib/colors.sh

for file in $(find . -type f -name "*.scm")
do
  echo -e "$Yel Compiling $file $RCol"
  echo ""
  echo '(cf "'$file'")' | scheme --quiet --eval
done
