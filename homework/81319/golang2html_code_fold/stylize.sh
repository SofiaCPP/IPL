#!/usr/bin/env bash
cat hello_world.go | ./parser > output.html
open output.html