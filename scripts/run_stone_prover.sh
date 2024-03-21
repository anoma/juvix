#!/bin/bash

BASE=`basename "$1" .json`

gen_stone_params.py ${BASE}_public_input.json > ${BASE}_params.json
cpu_air_prover --out_file=${BASE}_proof.json --private_input_file=${BASE}_private_input.json --public_input_file=${BASE}_public_input.json --prover_config_file=cpu_air_prover_config.json --parameter_file=${BASE}_params.json
