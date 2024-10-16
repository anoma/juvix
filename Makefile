SHELL := /bin/bash
PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)
EXAMPLEMILESTONE=examples/milestone
EXAMPLEHTMLOUTPUT=docs/examples/html
EXAMPLES= Collatz/Collatz.juvix \
	Fibonacci/Fibonacci.juvix \
	Hanoi/Hanoi.juvix \
	HelloWorld/HelloWorld.juvix \
	PascalsTriangle/PascalsTriangle.juvix \
	TicTacToe/CLI/TicTacToe.juvix \
	Bank/Bank.juvix \
	Tutorial/Tutorial.juvix

DEMO_EXAMPLE=examples/demo/Demo.juvix

MAKEAUXFLAGS?=-s
MAKE=make ${MAKEAUXFLAGS}
METAFILES:=README.md \
			 CHANGELOG.md \
			 CONTRIBUTING.md \
			 LICENSE.md

STACKFLAGS?=--jobs $(THREADS)
STACKTESTFLAGS?=--ta --hide-successes --ta --ansi-tricks=false --ta "+RTS -N -RTS"
SMOKEFLAGS?=--color --diff=git
STACK?=stack

JUVIXBIN?=juvix

CC=clang

ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

all: build

clean: clean-runtime
	@${STACK} clean --full
	@rm -rf .hie

.PHONY: clean-hard
clean-hard: clean
	@git clean -fdx

.PHONY: clean-runtime
clean-runtime: clean-juvix-build
	@cd runtime && ${MAKE} clean

.PHONY: clean-juvix-build
clean-juvix-build:
	@find . -type d -name '.juvix-build' | xargs rm -rf

repl:
	@${STACK} ghci Juvix:lib ${STACKFLAGS}

# -- EXAMPLES HTML OUTPUT

.PHONY: html-examples
html-examples: $(EXAMPLES)

$(EXAMPLES):
	$(eval OUTPUTDIR=$(EXAMPLEHTMLOUTPUT)/$(dir $@))
	@mkdir -p ${OUTPUTDIR}
	@${JUVIXBIN} html $(EXAMPLEMILESTONE)/$@ --output-dir=$(CURDIR)/${OUTPUTDIR}

.PHONY: demo-example
demo-example:
	$(eval OUTPUTDIR=$(EXAMPLEHTMLOUTPUT)/Demo)
	@mkdir -p ${OUTPUTDIR}
	@${JUVIXBIN} html $(DEMO_EXAMPLE) --output-dir=$(CURDIR)/${OUTPUTDIR}

.PHONY : haddock
haddock :
	@cabal --docdir=docs/ --htmldir=docs/ haddock --enable-documentation

# ------------------------------------------------------------------------------
# -- Codebase Health
# ------------------------------------------------------------------------------

ORMOLU?=${STACK} exec -- ormolu
ORMOLUFILES = $(shell git ls-files '*.hs' '*.hs-boot' | grep -v '^contrib/')
ORMOLUFLAGS?=--no-cabal
ORMOLUMODE?=inplace

.PHONY: ormolu
ormolu:
	@${ORMOLU} ${ORMOLUFLAGS} \
		--mode ${ORMOLUMODE} \
		$(ORMOLUFILES)

.PHONY: format
format:
	@${MAKE} clang-format
	@${MAKE} ormolu

.PHONY: clang-format
clang-format:
	@cd runtime && ${MAKE} format

JUVIX_PACKAGES_IN_REPO=$(shell find \
	./examples \
	./tests/positive \
	./tests/negative \
	-type d \( -name ".juvix-build" -o -name "FancyPaths" \) -prune -o \
	-type f -name 'Package.juvix' -print \
	| awk -F'/Package.juvix' '{print $$1}' | sort -u)

JUVIXFORMATFLAGS?=--in-place
JUVIXTYPECHECKFLAGS?=--log-level warn

.PHONY: format-juvix-files
format-juvix-files:
	@for p in $(JUVIX_PACKAGES_IN_REPO); do \
		${JUVIXBIN} format $(JUVIXFORMATFLAGS) "$$p" > /dev/null 2>&1; \
		exit_code=$$?; \
		if [ $$exit_code -eq 0 ]; then \
			echo "[OK] $$p is formatted"; \
		elif [[ $$exit_code -ne 0 && "$$p" == *"tests/"* ]]; then \
			echo "[CONTINUE] $$p is in tests directory."; \
		else \
			echo "[FAIL] $$p formatting failed" && exit 1; \
		fi; \
		done;

.PHONY: check-format-juvix-files
check-format-juvix-files:
	@JUVIXFORMATFLAGS=--check ${MAKE} format-juvix-files

JUVIXEXAMPLEFILES=$(shell find ./examples \
	-type d \( -name ".juvix-build" \) -prune -o \
	-name "*.juvix" -print)

.PHONY: typecheck-juvix-examples
typecheck-juvix-examples:
	@for file in $(JUVIXEXAMPLEFILES); do \
		${JUVIXBIN} typecheck "$$file" $(JUVIXTYPECHECKFLAGS); \
		exit_code=$$?; \
		if [ $$exit_code -eq 0 ]; then \
			echo "[OK] $$file typechecks"; \
		else \
			echo "[FAIL] Typecking failed for $$file" && exit 1; \
		fi; \
	done

.PHONY: check-ormolu
check-ormolu: export ORMOLUMODE = check
check-ormolu:
	@${MAKE} ormolu

PRECOMMIT := $(shell command -v pre-commit 2> /dev/null)

.PHONY : install-pre-commit
install-pre-commit :
	@$(if $(PRECOMMIT),, pip install pre-commit)

.PHONY : pre-commit
pre-commit :
	@pre-commit run --all-files

# ------------------------------------------------------------------------------
# -- Build-Install-Test-Release
# ------------------------------------------------------------------------------

.PHONY: check-only
check-only:
	@${MAKE} build \
		&& ${MAKE} install \
		&& ${MAKE} test \
		&& ${MAKE} smoke \
		&& ${MAKE} check-format-juvix-files \
		&& ${MAKE} typecheck-juvix-examples \
		&& ${MAKE} check-ormolu \
		&& export SKIP=ormolu,format-juvix-examples,typecheck-juvix-examples \
		&& ${MAKE} pre-commit

.PHONY: check
check: clean
	@${MAKE} check-only

.PHONY : bench
bench: runtime submodules
	@${STACK} bench ${STACKFLAGS}

# -- Build requirements

.PHONY: submodules
submodules:
	@git submodule sync
	@git submodule update --init --recursive

.PHONY: build
build: submodules runtime
	@${STACK} build ${STACKFLAGS}

.PHONY: fast-build
fast-build: submodules runtime
	@${STACK} build --fast ${STACKFLAGS}

.PHONY: configure
configure:
	CC=${CC} config/configure.sh

.PHONY: runtime
runtime: configure
	cd runtime && make

# -- Install

.PHONY : install
install: runtime submodules
	@${STACK} install ${STACKFLAGS}

.PHONY : fast-install
fast-install: runtime submodules
	@${STACK} install --fast ${STACKFLAGS}

# -- Testing

.PHONY : test
test: build runtime submodules
	@${STACK} test ${STACKFLAGS} ${STACKTESTFLAGS}

.PHONY : fast-test
fast-test: fast-build
	@${STACK} test --fast ${STACKFLAGS} ${STACKTESTFLAGS}

.PHONY : test-skip-slow
test-skip-slow:
	@${STACK} test ${STACKFLAGS} ${STACKTESTFLAGS} --ta '-p "! /slow tests/"'

.PHONY : fast-test-skip-slow
fast-test-skip-slow:
	@${STACK} test --fast ${STACKFLAGS} ${STACKTESTFLAGS} --ta '-p "! /slow tests/"'

SMOKE := $(shell command -v smoke 2> /dev/null)
SHA256SUM := $(shell command -v sha256sum 2> /dev/null)

.PHONY : smoke-only
smoke-only:
	@$(if $(SMOKE),, $(error "Smoke not found, please install it from https://github.com/jonaprieto/smoke"))
	@$(if $(SHA256SUM),, $(error "sha256sum not found, please install the GNU coreutils package (e.g {apt, brew} install coreutils)"))
	@smoke $(shell find tests -name '*.smoke.yaml')

.PHONY : smoke
smoke: install submodules
	@${MAKE} smoke-only

# -- Release

.PHONY : changelog
changelog :
	@github_changelog_generator

HYPERFINEBIN := $(shell command -v hyperfine 2> /dev/null)

.PHONY : hyperfine-benchmarks
hyperfine-benchmarks:
	@$(if $(HYPERFINEBIN),, $(error "hyperfine not found, please install it using cargo or from https://github.com/sharkdp/hyperfine"))
	cd bench/hyperfine && ${MAKE}
