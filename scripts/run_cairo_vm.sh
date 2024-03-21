#!/bin/bash

BASE=`basename "$1" .json`

cairo-vm-cli "$1" --print_output --proof_mode --trace_file ${BASE}.trace --air_public_input=${BASE}_public_input.json --air_private_input=${BASE}_private_input.json --memory_file=${BASE}_memory.mem --layout=small
