#!/bin/bash

function execute() {
    echo $1
    eval $1
}

for d in *
do
    if [ -d "$d" ]; then
        echo
        echo "Compiling $d..."
        echo
        cd $d
        if [ -d haskell ]; then
            cd haskell
            for f in *.hs; do
                execute "ghc -O2 -o `basename $f .hs`.exe $f"
                rm *.hi *.o
                execute "ghc -XStrict -O2 -o `basename $f .hs`.strict.exe $f"
                rm *.hi *.o
            done
            cd ..
        fi
        if [ -d ocaml ]; then
            cd ocaml
            for f in *.ml; do
                execute "ocamlopt -O2 -o `basename $f .ml`.exe $f"
                execute "ocamlc -o `basename $f .ml`.byte.exe $f"
            done
            cd ..
        fi
        if [ -d juvix ]; then
            cd juvix
            for f in *.juvix; do
                execute "juvix compile -o `basename $f .juvix`.exe $f"
                execute "juvix compile --target=wasm -o `basename $f .juvix`.wasm $f"
            done
            cd ..
        fi
        if [ -d runtime ]; then
            cd runtime
            for f in *.c; do
                execute "juvix dev runtime compile -o `basename $f .c`.exe $f"
                execute "juvix dev runtime compile --target=wasm32-wasi -o `basename $f .c`.wasm $f"
            done
            cd ..
        fi
        if [ -d c ]; then
            cd c
            for f in *.c; do
                execute "clang -O3 -o `basename $f .c`.exe $f"
                execute "clang -Os --target=wasm32-wasi --sysroot $WASI_SYSROOT_PATH -o `basename $f .c`.wasm $f"
            done
            cd ..
        fi
        cd ..
    fi
done
