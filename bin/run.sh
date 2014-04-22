#!/bin/bash
set -e

source lib/colors.sh

PASS_COUNT=0
FAIL_COUNT=0

# Run scheme files or compiled objects.
if [[ $@ =~ '--so' ]] || [[ $@ =~ '-so' ]]
then
  if [ -f lib/assert.com ]
  then
    FILE_TYPE=com
  else
    FILE_TYPE=so
  fi
else
  FILE_TYPE=scm
fi

for file in $(find src -type f -name "*.$FILE_TYPE")
do
  echo -e "$Yel Running Problem Set: $file $RCol"
  echo ""

  declare -a RESULTS
  RESULTS=$(echo '' | scheme --band runtime.com --quiet --load $file)

  OIFS=$IFS
  IFS=$'\n'
  for result in $RESULTS
  do
    if [[ $result == *PASSED* ]]
    then
      PASS_COUNT=$(($PASS_COUNT + 1))
      color=$Gre
      if [ $VERBOSE ];
      then
        $result = ''
      fi
    elif [[ $result == *FAILED* ]]
    then
      FAIL_COUNT=$(($FAIL_COUNT + 1))
      color=$Red
      if [ $HIDE == 'true' ];
      then
        echo -e "$color $result $RCol\n"
      fi
    elif [[ $result == *TEST* ]]
    then
      color=$Cya
    fi

    if [ $HIDE == 'false' ];
    then
      echo -e "$color $result $RCol\n"
    fi
  done
  IFS=$OIFS
done

TOTAL_COUNT=$(($PASS_COUNT + $FAIL_COUNT))
echo    ""
echo    "  --------------------------------------------------------"
echo -e "  $TOTAL_COUNT total tests ran: $PASS_COUNT tests passed, and $FAIL_COUNT tests failed."
echo    ""
test $FAIL_COUNT -eq 0
