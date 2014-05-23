#!/bin/bash
set -e

source lib/colors.sh

PASS_COUNT=0
FAIL_COUNT=0
if [[ -z "$SICP_MODE" ]];
then
  MODE='verbose';
else 
  MODE="$SICP_MODE"
fi

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

# Run all src files or just one.
FILE='*'
if [[ $@ =~ [1-9]+\.[0-9]+ ]];
then
  FILE=$@
fi

for file in $(find src -type f -name "$FILE.$FILE_TYPE" | sort)
do
  echo -e "$Yel Running Problem Set: $file $RCol"
  echo ""

  declare -a RESULTS
  RESULTS=$(echo '' | scheme --band runtime.com --quiet --load 'config.scm' $file)

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

    if [ "$MODE" == 'verbose' ];
    then
      echo -e "$color $result $RCol\n"
    else
      if [ $color == $Red ];
      then
        echo -e "$color $result $RCol\n"
      fi
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
