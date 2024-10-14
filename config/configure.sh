#!/usr/bin/env bash

# This script should be run from the root of the project

if [ `basename "$PWD"` != "juvix" ] || [ ! -d "config" ]; then
    echo "This script should be run from the root of the project"
    exit 1
fi

if clang -target wasm32-wasi --print-supported-cpus >/dev/null 2>&1; then
    WASM="true"
else
    WASM="false"
fi

if rustc --version >/dev/null 2>&1; then
    RUST="true"
else
    RUST="false"
fi

printf "{\n    \"wasm\": $WASM,\n    \"rust\": $RUST\n}\n" > config/config.json
