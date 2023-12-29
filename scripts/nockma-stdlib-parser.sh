#!/usr/bin/env bash
# Usage:
# chmod +x nockma-stdlib-parser.sh
# ./nockma-stdlib-parser < anoma.hoon

# Use grep to find lines matching the pattern and awk to format the output
# Reads from stdin
grep -oP '\+\+  \K\w+\s+::\s+\+\d+' | awk '{gsub("::  \\+", ""); print}'
