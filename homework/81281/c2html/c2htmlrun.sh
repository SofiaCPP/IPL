#!/bin/sh

echo -n "Please enter the name of the C file you wish to highlight: "
read fileName
echo -ne "Please enter:\n\t0 for highlighting\n\t1 highlighting and prettification\n\t2 for highlighting and prettification and control structure collapse: "
read number
if ! [[ $fileName == *.c\n ]]; then
    base=`pwd`"/c2html.pl"
    PL=swipl
    exec $PL -q -f "$base" "$fileName" "$number"
else
    echo "Not a correct C filename!"
fi
