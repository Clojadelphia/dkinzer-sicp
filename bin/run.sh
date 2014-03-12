#!/bin/bash
set -e

source ./lib/colors.sh

PASS_COUNT=0
FAIL_COUNT=0

# Run scheme files or compiled objects.
if [[ $@ =~ '--so' ]] || [[ $@ =~ '-so' ]]
then
  FILE_TYPE=so
else
  FILE_TYPE=scm
fi

for file in $(ls ./src/*.$FILE_TYPE)
do
  echo -e "$Yel Running Problem Set: $file $RCol"
  echo ""

  declare -a RESULTS
  RESULTS=$(echo '' | scheme --quiet --load $file)

  OIFS=$IFS
  IFS=$'\n'
  for result in $RESULTS
  do
    if [[ $result == *PASSED* ]]
    then
      PASS_COUNT=$(($PASS_COUNT + 1))
      color=$Gre
    elif [[ $result == *FAILED* ]]
    then
      FAIL_COUNT=$(($FAIL_COUNT + 1))
      color=$Red
    elif [[ $result == *TEST* ]]
    then
      color=$Cya
    fi
    echo -e "$color $result $RCol\n"
  done
  IFS=$OIFS
done

TOTAL_COUNT=$(($PASS_COUNT + $FAIL_COUNT))
echo    ""
echo    "  --------------------------------------------------------"
echo -e "  $TOTAL_COUNT total tests ran: $PASS_COUNT tests passed, and $FAIL_COUNT tests failed."
echo    ""
test $FAIL_COUNT -eq 0
