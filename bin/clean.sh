#!/bin/bash
# @file
# clean.sh
#
# Deletes all files associated to the compile  process.

BUILD_FILE_EXTENSIONS=( o c so bin bci com )

for ext in ${BUILD_FILE_EXTENSIONS[@]}
do
  find . -type f -name "*.$ext" -exec rm {} \;
done
