#!/usr/bin/env bash

# USAGE: Set the variable ANOMA_PATH to the root of the anoma clone

set -e

SCRIPT_DIR="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

if [[ -z "$ANOMA_PATH" ]]; then
    echo "ANOMA_PATH must be set to the root of the anoma clone"
    exit 1
fi

if [[ ! -d "$ANOMA_PATH" ]]; then
    echo "Error: Directory ANOMA_PATH (set to '$ANOMA_PATH') does not exist." >&2
    exit 1
fi

cd "$ANOMA_PATH"
mix deps.get
mix compile
mix run "$SCRIPT_DIR/write-anomalib.exs"
juvix dev nockma format "$ANOMA_PATH/anomalib.nockma" > "$SCRIPT_DIR/anomalib.nockma"
echo "anomalib.nockma successfully updated"
