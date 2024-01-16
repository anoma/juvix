#!/usr/bin/env bash
#
# Description:
# A script that extracts the Nock locations of functions from comments in the the
# anoma.hoon standard library:
#
# For example we want to extract the location "342" for the function dec.:
# https://github.com/anoma/anoma/blob/9904ff81218c1a690027a481beb0b6d39e378a07/hoon/anoma.hoon#L12
# ```
# ++  dec  ::  +342
# ```
#
# Usage:
# chmod +x nockma-stdlib-parser.sh
# ./nockma-stdlib-parser.sh < anoma.hoon

# Use grep to find lines matching the pattern and awk to format the output
# Reads from stdin
grep -oP '\+\+  \K\w+\s+::\s+\+\d+' | awk '{gsub("::  \\+", ""); print}'
