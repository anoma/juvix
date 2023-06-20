#!/bin/bash

function count() {
    cloc $1 | grep 'SUM:' | awk '{print $5}'
}

RUNTIME=$(count runtime/src)

BACKENDC=$(count src/Juvix/Compiler/Backend/C/)
GEB=$(count src/Juvix/Compiler/Backend/Geb/)
VAMPIR=$(count src/Juvix/Compiler/Backend/VampIR/)
REG=$(count src/Juvix/Compiler/Reg/)
ASM=$(count src/Juvix/Compiler/Asm/)
CORE=$(count src/Juvix/Compiler/Core/)

CONCRETE=$(count src/Juvix/Compiler/Concrete/)
ABSTRACT=$(count src/Juvix/Compiler/Abstract/)
INTERNAL=$(count src/Juvix/Compiler/Internal/)
BUILTINS=$(count src/Juvix/Compiler/Builtins/)
PIPELINE=$(count src/Juvix/Compiler/Pipeline/)

APP=$(count app/)
HTML=$(count src/Juvix/Compiler/Backend/Html/)
EXTRA=$(count src/Juvix/Extra/)
DATA=$(count src/Juvix/Data/)
PRELUDE=$(count src/Juvix/Prelude/)

FRONT=$((CONCRETE + ABSTRACT + INTERNAL + BUILTINS + PIPELINE))
BACK=$((BACKENDC + GEB + VAMPIR + REG + ASM + CORE))
OTHER=$((APP + HTML + EXTRA + DATA + PRELUDE))
TESTS=$(count test/)

TOTAL=$((FRONT+BACK+OTHER+TESTS))

echo "Front end: $FRONT LOC"
echo "   Concrete: $CONCRETE LOC"
echo "   Abstract: $ABSTRACT LOC"
echo "   Internal: $INTERNAL LOC"
echo "   Builtins: $BUILTINS LOC"
echo "   Pipeline: $PIPELINE LOC"
echo "Middle and back end: $BACK LOC"
echo "   VampIR backend: $VAMPIR LOC"
echo "   GEB backend: $GEB LOC"
echo "   C backend: $BACKENDC LOC"
echo "   JuvixReg: $REG LOC"
echo "   JuvixAsm: $ASM LOC"
echo "   JuvixCore: $CORE LOC"
echo "Runtime: $RUNTIME LOC"
echo "Other: $OTHER LOC"
echo "   Application: $APP LOC"
echo "   Html: $HTML LOC"
echo "   Extra: $EXTRA LOC"
echo "   Data: $DATA LOC"
echo "   Prelude: $PRELUDE LOC"
echo "Tests: $TESTS LOC"
echo ""
echo "Total: $TOTAL Haskell LOC + $RUNTIME C LOC"
