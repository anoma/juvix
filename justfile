# set to non-empty string to disable parallel tests
#
# e.g:
#   just disableParallel=yes test
disableParallel := ''

# set to non-empty string to enable optimzed build
#
# e.g:
#   just enableOptimized=yes install
enableOptimized := ''

# set to non-empty string to enable command debugging
enableDebug := ''

# the command used to run stack
stack := "stack"

# the command used to run ormolu
ormolu := "ormolu"

# flags used in the stack command
stackOptFlag := if enableOptimized == '' { '--fast' } else { '' }
stackArgs := stackOptFlag + " -j" + num_cpus()

# flags used in the stack test command
testArgs := "--hide-successes"
rtsFlag := if disableParallel == '' { '+RTS -N -RTS' } else { '' }

# flag used to enable tracing of bash commands
bashDebugArg := if enableDebug == '' { '' } else { 'x' }

[private]
default:
    @just --list

@_ormoluCmd filesCmd:
    {{ trim(filesCmd) }} \
     | xargs {{ ormolu }} --no-cabal \
     --ghc-opt -XStandaloneDeriving \
     --ghc-opt -XUnicodeSyntax \
     --ghc-opt -XDerivingStrategies \
     --ghc-opt -XPatternSynonyms \
     --ghc-opt -XMultiParamTypeClasses  \
     --ghc-opt -XTemplateHaskell \
     --ghc-opt -XImportQualifiedPost \
     --mode inplace

# Formats all Haskell files in the project. `format changed` formats only changed files. `format FILES` formats individual files.
format *opts:
    #!/usr/bin/env bash
    set -euo{{ bashDebugArg }} pipefail

    opts='{{ trim(opts) }}'

    case $opts in
        "")
            just _ormoluCmd "git ls-files '*.hs'"
            ;;
        changed)
            just _ormoluCmd \
              "(git --no-pager diff --name-only --diff-filter=AM && git --no-pager diff --cached --name-only --diff-filter=AM) | grep '\\.hs\$'"
            ;;
        *)
            just _ormoluCmd "echo {{ opts }}"
            ;;
    esac

# Run the tests in the project. Use the filter arg to set a Tasty pattern.
[no-exit-message]
test *filter:
    #!/usr/bin/env bash
    set -euo{{ bashDebugArg }} pipefail
    filter='{{ trim(filter) }}'
    if [ -n "$filter" ]; then
      filter="-p \"$filter\""
    fi
    {{ stack }} test {{ stackArgs }} --ta "{{ testArgs }} {{ rtsFlag }} $filter"

# Run a juvix command and profile it
run-profile +cmd:
    cabal run --enable-profiling juvix -- {{ cmd }} +RTS -p

# Build the juvix runtime
_buildRuntime:
    cd runtime && make -j 4 -s

# Build the project. `build runtime` builds only the runtime.
[no-exit-message]
build *opts:
    #!/usr/bin/env bash
    set -euo{{ bashDebugArg }} pipefail
    opts='{{ trim(opts) }}'

    case $opts in
        runtime)
            just _buildRuntime
            ;;
        *)
            just _buildRuntime
            {{ stack }} build {{ stackArgs }}
            ;;
    esac

# Install juvix
[no-exit-message]
install: _buildRuntime
    {{ stack }} install {{ stackArgs }}

# Clean all .juvix-build directories in the project
_cleanJuvixBuild:
    @find . -type d -name '.juvix-build' | xargs rm -rf

# Clean the juvix runtime build directory
_cleanRuntime:
    @cd runtime && make clean

# Clean the project. `clean runtime` cleans only the runtime. `clean juvix-build` cleans only juvix-build dirs.
clean *opts:
    #!/usr/bin/env bash
    set -euo{{ bashDebugArg }} pipefail
    opts='{{ trim(opts) }}'

    case $opts in
        runtime)
            just _cleanRuntime
            ;;
        juvix-build)
            just _cleanJuvixBuild
            ;;
        *)
            just _cleanRuntime
            just _cleanJuvixBuild
            {{ stack }} clean --full
            ;;
    esac
