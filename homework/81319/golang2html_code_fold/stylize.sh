#!/usr/bin/env bash
cat input.go | ./parser > output.html
open output.html