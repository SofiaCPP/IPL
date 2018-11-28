#!/bin/sh

echo -ne "Please enter 0 for live interpreter or 1 for interpretation of a file: "
read mode

base=`pwd`"/interpreterMain.pl"

if [ $mode == 0 ]
then
    exec swipl -q -f "$base" ""
else
    echo -n "Please enter the name of the file: "
    read fileName
    exec swipl -q -f "$base" "$fileName"
fi
