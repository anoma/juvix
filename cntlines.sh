#!/bin/bash

RUNTIME=`find runtime/src/ -name '*.c' -or -name '*.h' | xargs wc -l | tail -1 | tr -d ' toal'`

BACKENDC=`find src/Juvix/Compiler/Backend/C/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
GEB=`find src/Juvix/Compiler/Backend/Geb/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
REG=`find src/Juvix/Compiler/Reg/ -name '*.hs' -print | xargs wc -l | tail -1 | tr -d ' toal'`
ASM=`find src/Juvix/Compiler/Asm/ -name '*.hs' -print | xargs wc -l | tail -1 | tr -d ' toal'`
CORE=`find src/Juvix/Compiler/Core/ -name '*.hs' -print | xargs wc -l | tail -1 | tr -d ' toal'`

CONCRETE=`find src/Juvix/Compiler/Concrete/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
ABSTRACT=`find src/Juvix/Compiler/Abstract/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
INTERNAL=`find src/Juvix/Compiler/Internal/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
BUILTINS=`find src/Juvix/Compiler/Builtins/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
PIPELINE=`find src/Juvix/Compiler/Pipeline/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`

APP=`find app/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
HTML=`find src/Juvix/Compiler/Backend/Html/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
EXTRA=`find src/Juvix/Extra/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
DATA=`find src/Juvix/Data/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`
PRELUDE=`find src/Juvix/Prelude/ -name '*.hs' | xargs wc -l | tail -1 | tr -d ' toal'`

FRONT=$((CONCRETE + ABSTRACT + INTERNAL + BUILTINS + PIPELINE))
BACK=$((BACKENDC + GEB + REG + ASM + CORE))
OTHER=$((APP + HTML + EXTRA + DATA + PRELUDE))

echo "Front end: $FRONT LOC"
echo "   Concrete: $CONCRETE LOC"
echo "   Abstract: $ABSTRACT LOC"
echo "   Internal: $INTERNAL LOC"
echo "   Builtins: $BUILTINS LOC"
echo "   Pipeline: $PIPELINE LOC"
echo "Middle and back end: $BACK LOC"
echo "   C backend: $BACKENDC LOC"
echo "   GEB backend: $GEB LOC"
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
