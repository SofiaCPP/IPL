#!/bin/bash

g++ -Wall -O2 -std=c++11 -c py2html.cpp -o py2html.o
g++ -Wall -O2 -std=c++11 -c Token.cpp -o Token.o
g++  -o py2html py2html.o  Token.o  -s
