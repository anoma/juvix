#!/bin/bash

function count() {
    cloc $1 | grep 'SUM:' | awk '{print $5}'
}

function count_ext () {
    find $2 -name $1 -print | xargs sed '/^[[:space:]]*$/d' | wc -l | tr -d ' '
}

RUNTIME_C=$(count runtime/src/juvix)
RUNTIME_VAMPIR=$(count_ext '*.pir' runtime/src/vampir)
RUNTIME_JVA=$(count_ext '*.jva' runtime/src/asm)

RUNTIME=$((RUNTIME_C+RUNTIME_VAMPIR+RUNTIME_JVA))

BACKENDC=$(count src/Juvix/Compiler/Backend/C/)
CAIRO=$(count src/Juvix/Compiler/Backend/Cairo/)
GEB=$(count src/Juvix/Compiler/Backend/Geb/)
VAMPIR=$(count src/Juvix/Compiler/Backend/VampIR/)
CASM=$(count src/Juvix/Compiler/Casm/)
REG=$(count src/Juvix/Compiler/Reg/)
ASM=$(count src/Juvix/Compiler/Asm/)
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
BACK=$((BACKENDC + GEB + VAMPIR + REG + ASM + CORE + CASM + CAIRO))
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
echo "   JuvixReg: $REG LOC"
echo "   JuvixAsm: $ASM LOC"
echo "   JuvixCore: $CORE LOC"
echo "Runtime: $RUNTIME LOC"
echo "   C runtime: $RUNTIME_C LOC"
echo "   JuvixAsm runtime: $RUNTIME_JVA LOC"
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
echo "Total: $TOTAL Haskell LOC + $RUNTIME_C C LOC + $RUNTIME_JVA JuvixAsm LOC + $RUNTIME_VAMPIR VampIR LOC"
