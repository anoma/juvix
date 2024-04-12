#!/usr/bin/env bash

BASE=`basename "$1" .json`

juvix-cairo-vm "$@" --print_output --proof_mode --trace_file ${BASE}.trace --air_public_input=${BASE}_public_input.json --air_private_input=${BASE}_private_input.json --memory_file=${BASE}_memory.mem --layout=all_cairo
