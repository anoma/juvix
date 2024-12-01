#!/usr/bin/env bash

if [ ! -d "config" ]; then
    printf "This script should be run from the root of the project.\n" 1>&2
    exit 1
fi

if [ -z "$( ls -A 'juvix-stdlib' )" ]; then
    printf "Empty standard library directory.\nDid you remember to clone with the --recursive option?\nTry running:\n   git submodule init\n   git submodule update\n" 1>&2
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

cat <<EOF > config/config.json
{
  "wasm": $WASM,
  "rust": $RUST,
  "clang": "$CC",
  "cargo": "$CARGO"
}
EOF
