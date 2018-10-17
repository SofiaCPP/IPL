#!/bin/sh

echo -n "Please enter the name of the C file you wish to highlight:"
read fileName
echo $fileName
if ! [[ $fileName == *.c\n ]]; then
    base=`pwd`"/c2html.pl"
    PL=swipl
    exec $PL -q -f "$base" "$fileName"
else
    echo "Not a correct C filename!"
fi
