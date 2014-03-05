#!/bin/bash

# Sometimes these processes get stuck in an infinite loop. It's nice to be able
# to just kill them all.

for pid in $(ps x -c | grep scheme | cut -f 1 -d ' ')
do
  echo About to kill process $pid.
  kill -9 $pid
done
