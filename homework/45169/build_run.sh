#!/bin/sh

make clean && \
make && \
./python2html example.py > result.html
