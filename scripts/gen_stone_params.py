#!/usr/bin/env python
#
# This script generates appropriate Stone prover parameters with the right
# bounds for the number of execution steps.
#
# Invocation: gen_stone_params.py prog_public_input.json
#

import json
import sys
import math

if len(sys.argv) != 2:
    sys.exit("Usage: gen_stone_params.py prog_public_input.json")

f = open(sys.argv[1])
data = json.load(f)
f.close()

n = int(math.log2(data["n_steps"])) - 6
if n < 0:
    sys.exit("Too few execution steps (at least 64 required)")

fri_step_list = [0, 4]
while n > 3:
    fri_step_list.append(3)
    n -= 3
if n > 0:
    fri_step_list.append(n)

out = {
    "field": "PrimeField0",
    "stark": {
        "fri": {
            "fri_step_list": fri_step_list,
            "last_layer_degree_bound": 64,
            "n_queries": 18,
            "proof_of_work_bits": 24,
        },
        "log_n_cosets": 4,
    },
    "use_extension_field": False,
}

print(json.dumps(out))
