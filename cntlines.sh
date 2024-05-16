#!/bin/bash

function count() {
    cloc $1 | grep 'SUM:' | awk '{print $5}'
}

function count_ext () {
    find $2 -name $1 -print | xargs sed '/^[[:space:]]*$/d' | wc -l | tr -d ' '
}

RUNTIME_C=$(count runtime/c/src/juvix)
RUNTIME_RUST=$(count runtime/rust/src)
RUNTIME_VAMPIR=$(count_ext '*.pir' runtime/vampir)
RUNTIME_JVT=$(count_ext '*.jvt' runtime/tree)
RUNTIME_CASM=$(count_ext '*.casm' runtime/casm)

RUNTIME=$((RUNTIME_C+RUNTIME_RUST+RUNTIME_VAMPIR+RUNTIME_JVT+RUNTIME_CASM))

BACKENDC=$(count src/Juvix/Compiler/Backend/C/)
CAIRO=$(count src/Juvix/Compiler/Backend/Cairo/)
GEB=$(count src/Juvix/Compiler/Backend/Geb/)
VAMPIR=$(count src/Juvix/Compiler/Backend/VampIR/)
CASM=$(count src/Juvix/Compiler/Casm/)
NOCK=$(count src/Juvix/Compiler/Nockma)
REG=$(count src/Juvix/Compiler/Reg/)
ASM=$(count src/Juvix/Compiler/Asm/)
TREE=$(count src/Juvix/Compiler/Tree/)
CORE=$(count src/Juvix/Compiler/Core/)

CONCRETE=$(count src/Juvix/Compiler/Concrete/)
INTERNAL=$(count src/Juvix/Compiler/Internal/)
BUILTINS=$(count src/Juvix/Compiler/Builtins/)
PIPELINE=$(count src/Juvix/Compiler/Pipeline/)

APP=$(count app/)
HTML=$(count src/Juvix/Compiler/Backend/Html/)
EXTRA=$(count src/Juvix/Extra/)
DATA=$(count src/Juvix/Data/)
PRELUDE=$(count src/Juvix/Prelude/)
STORE=$(count src/Juvix/Compiler/Store/)

FRONT=$((CONCRETE + INTERNAL + BUILTINS + PIPELINE))
BACK=$((BACKENDC + GEB + VAMPIR + NOCK + REG + ASM + TREE + CORE + CASM + CAIRO))
OTHER=$((APP + STORE + HTML + EXTRA + DATA + PRELUDE))
TESTS=$(count test/)

TOTAL=$((FRONT+BACK+OTHER+TESTS))

echo "Front end: $FRONT LOC"
echo "   Concrete: $CONCRETE LOC"
echo "   Internal: $INTERNAL LOC"
echo "   Builtins: $BUILTINS LOC"
echo "   Pipeline: $PIPELINE LOC"
echo "Middle and back end: $BACK LOC"
echo "   VampIR backend: $VAMPIR LOC"
echo "   GEB backend: $GEB LOC"
echo "   C backend: $BACKENDC LOC"
echo "   Cairo backend: $((CASM + CAIRO)) LOC"
echo "   Nockma backend: $NOCK LOC"
echo "   JuvixReg: $REG LOC"
echo "   JuvixAsm: $ASM LOC"
echo "   JuvixTree: $TREE LOC"
echo "   JuvixCore: $CORE LOC"
echo "Runtime: $RUNTIME LOC"
echo "   C runtime: $RUNTIME_C LOC"
echo "   Rust runtime: $RUNTIME_RUST LOC"
echo "   JuvixTree runtime: $RUNTIME_JVT LOC"
echo "   Cairo assembly runtime: $RUNTIME_CASM LOC"
echo "   VampIR runtime: $RUNTIME_VAMPIR LOC"
echo "Other: $OTHER LOC"
echo "   Application: $APP LOC"
echo "   Store: $STORE LOC"
echo "   Html: $HTML LOC"
echo "   Extra: $EXTRA LOC"
echo "   Data: $DATA LOC"
echo "   Prelude: $PRELUDE LOC"
echo "Tests: $TESTS LOC"
echo ""
echo "Total: $TOTAL Haskell LOC + $RUNTIME_C C LOC + $RUNTIME_RUST Rust LOC + $RUNTIME_JVT JuvixTree LOC + $RUNTIME_CASM CASM LOC + $RUNTIME_VAMPIR VampIR LOC"
