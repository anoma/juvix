PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)

EXAMPLEMILESTONE=examples/milestone
EXAMPLEHTMLOUTPUT=_docs/examples/html
EXAMPLES= Collatz/Collatz.juvix \
	Fibonacci/Fibonacci.juvix \
	Hanoi/Hanoi.juvix \
	HelloWorld/HelloWorld.juvix \
	PascalsTriangle/PascalsTriangle.juvix \
	TicTacToe/CLI/TicTacToe.juvix \
	Tutorial/Tutorial.juvix \

DEMO_EXAMPLE=examples/demo/Demo.juvix

MAKEAUXFLAGS?=-s
MAKE=make ${MAKEAUXFLAGS}
METAFILES:=README.md \
		   CHANGELOG.md \
		   CONTRIBUTING.md \
		   LICENSE.md

ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

clean: clean-runtime
	@stack clean --full
	@rm -rf .hie
	@rm -rf book

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
	@stack ghci Juvix:lib

# ------------------------------------------------------------------------------
# -- The Juvix Book
# ------------------------------------------------------------------------------

# -- EXAMPLES

.PHONY: html-examples
html-examples: $(EXAMPLES)

$(EXAMPLES):
	$(eval OUTPUTDIR=$(EXAMPLEHTMLOUTPUT)/$(dir $@))
	@mkdir -p ${OUTPUTDIR}
	@juvix html $(EXAMPLEMILESTONE)/$@ --output-dir=$(CURDIR)/${OUTPUTDIR}

.PHONY: demo-example
demo-example:
	$(eval OUTPUTDIR=$(EXAMPLEHTMLOUTPUT)/Demo)
	@mkdir -p ${OUTPUTDIR}
	@juvix html $(DEMO_EXAMPLE) --output-dir=$(CURDIR)/${OUTPUTDIR}

# -- MDBook

.PHONY: docs
docs:
	@cp $(METAFILES) docs/
	@mdbook build

.PHONY: serve-docs
serve-docs: docs
	@mdbook serve --open

cargo-dependencies:
	@cargo install mdbook \
				   mdbook-katex \
				   mdbook-linkcheck \
				   mdbook-pagetoc

# -- Codebase Documentation

.PHONY : haddock
haddock :
	@cabal --docdir=docs/ --htmldir=docs/ haddock --enable-documentation

# ------------------------------------------------------------------------------
# -- Codebase Health
# ------------------------------------------------------------------------------

ORMOLU?=stack exec -- ormolu
ORMOLUFILES = $(shell git ls-files '*.hs' '*.hs-boot' | grep -v '^contrib/')
ORMOLUFLAGS?=--no-cabal
ORMOLUMODE?=inplace

.PHONY: ormolu
ormolu:
	@${ORMOLU} ${ORMOLUFLAGS} \
		--ghc-opt -XStandaloneDeriving \
		--ghc-opt -XUnicodeSyntax \
		--ghc-opt -XDerivingStrategies \
		--ghc-opt -XMultiParamTypeClasses  \
		--ghc-opt -XTemplateHaskell \
		--ghc-opt -XImportQualifiedPost \
			--mode ${ORMOLUMODE} \
		$(ORMOLUFILES)

.PHONY: format
format:
	@${MAKE} clang-format
	@${MAKE} ormolu

.PHONY: clang-format
clang-format:
	@cd runtime && ${MAKE} format

JUVIXEXAMPLEFILES=$(shell find ./examples -name "*.juvix" -print)
JUVIXFORMATFLAGS?=--in-place
JUVIXTYPECHECKFLAGS?=--only-errors

.PHONY: format-juvix-examples
format-juvix-examples:
	@for file in $(JUVIXEXAMPLEFILES); do \
		juvix format $(JUVIXFORMATFLAGS) "$$file"; \
	done

.PHONY: check-format-juvix-examples
check-format-juvix-examples:
	@export JUVIXFORMATFLAGS=--check
	@${MAKE} format-juvix-examples

.PHONY: typecheck-juvix-examples
typecheck-juvix-examples:
	@for file in $(JUVIXEXAMPLEFILES); do \
		echo "Checking $$file"; \
		juvix typecheck "$$file" $(JUVIXTYPECHECKFLAGS); \
	done

.PHONY: check-ormolu
check-ormolu: export ORMOLUMODE = check
check-ormolu:
	@${MAKE} ormolu

HLINT?=stack exec -- hlint
HLINTFLAGS?=
HLINTQUIET :=

.PHONY : hlint
hlint :
	${HLINT} ${HLINTFLAGS} app ${HLINTQUIET}
	${HLINT} ${HLINTFLAGS} src ${HLINTQUIET}
	${HLINT} ${HLINTFLAGS} test ${HLINTQUIET}

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

STACKFLAGS?=--jobs $(THREADS)
STACKTESTFLAGS?=--ta --hide-successes --ta --ansi-tricks=false
SMOKEFLAGS?=--color --diff=git

.PHONY: check-only
check-only:
	@${MAKE} build
	@${MAKE} install
	@${MAKE} test
	@${MAKE} smoke
	@${MAKE} check-juviformat
	@${MAKE} format
	@${MAKE} pre-commit

.PHONY: check
check: clean
	@${MAKE} check-only

# -- Build requirements

.PHONY: submodules
submodules:
	@git submodule sync
	@git submodule update --init --recursive

.PHONY: build
build: submodules runtime
	stack build ${STACKFLAGS}

.PHONY: fast-build
fast-build: submodules runtime
	stack build --fast ${STACKFLAGS}

.PHONY: runtime
runtime:
	cd runtime && make -j 4 -s

# -- Install

.PHONY : install
install: runtime submodules
	@stack install ${STACKFLAGS}

.PHONY : fast-install
fast-install: runtime submodules
	@stack install --fast ${STACKFLAGS}

# -- Testing

.PHONY : test
test: build runtime submodules
	@stack test ${STACKFLAGS} ${STACKTESTFLAGS}

.PHONY : fast-test
fast-test: fast-build
	@stack test --fast ${STACKFLAGS} ${STACKTESTFLAGS}

.PHONY : test-skip-slow
test-skip-slow:
	@stack test ${STACKFLAGS} ${STACKTESTFLAGS} --ta '-p "! /slow tests/"'

.PHONY : fast-test-skip-slow
fast-test-skip-slow:
	@stack test --fast ${STACKFLAGS} ${STACKTESTFLAGS} --ta '-p "! /slow tests/"'

SMOKE := $(shell command -v smoke 2> /dev/null)

.PHONY : smoke-only
smoke-only:
	@$(if $(SMOKE),, $(error "Smoke not found, please install it from https://github.com/jonaprieto/smoke"))
	@smoke $(shell find tests -name '*.smoke.yaml')

.PHONY : smoke
smoke: install submodules
	@${MAKE} smoke-only

# -- Release

.PHONY : changelog-updates
changelog-updates :
	@github_changelog_generator
	@pandoc CHANGELOG.md --from markdown --to org -o UPDATES-FOR-CHANGELOG.org

.PHONY : bench
bench: runtime submodules
	@stack bench
