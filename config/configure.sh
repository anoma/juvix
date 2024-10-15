#!/usr/bin/env bash

# This script should be run from the root of the project

if [ ! -d "config" ]; then
    printf "This script should be run from the root of the project.\n" 1>&2
    exit 1
fi

if [ -z "$CC" ]; then
    CC="clang"
fi

if [ -z "$CARGO" ]; then
    CARGO="cargo"
fi

if $CC -target wasm32-wasi --print-supported-cpus >/dev/null 2>&1; then
    WASM="true"
else
    WASM="false"
fi

if $CARGO --version >/dev/null 2>&1; then
    RUST="true"
else
    RUST="false"
fi

printf "{\n  \"wasm\": $WASM,\n  \"rust\": $RUST,\n  \"clang\": \"$CC\",\n  \"cargo\": \"$CARGO\"\n}\n" > config/config.json
