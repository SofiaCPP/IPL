#!/bin/bash

flex $1
gcc $2 -o wrenHL.exe
./wrenHL.exe $3 > $4
