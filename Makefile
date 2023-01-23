PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)

IMAGES = $(shell find assets/images -type f)

ORGFILES = $(shell find docs/org -type f -name '*.org')
MDFILES:=$(patsubst docs/org/%,docs/md/%,$(ORGFILES:.org=.md))

EXAMPLEMILESTONE=examples/milestone
EXAMPLEHTMLOUTPUT=_docs/examples/html
EXAMPLES= Collatz/Collatz.juvix \
	Fibonacci/Fibonacci.juvix \
	Hanoi/Hanoi.juvix \
	HelloWorld/HelloWorld.juvix \
	PascalsTriangle/PascalsTriangle.juvix \
	TicTacToe/CLI/TicTacToe.juvix \
	Tutorial/Tutorial.juvix \
	ValidityPredicates/SimpleFungibleToken.juvix

EXAMPLE_WEBAPP_OUTPUT=_docs/examples/webapp
WEBAPP_EXAMPLES=TicTacToe/Web/TicTacToe.juvix
DEMO_EXAMPLE=examples/demo/Demo.juvix

ORGTOMDPRG ?=pandoc
ORGOPTS=--from org --to markdown_strict -s -o $@

ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

images:
	echo $(IMAGES)

all: install

clean: clean-runtime
	@stack clean --full
	@rm -rf .hie
	@rm -rf _docs
	@rm -rf docs/md

.PHONY: clean-runtime
clean-runtime: clean-juvix-build
	@cd runtime && make clean

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

.PHONY: webapp-examples
webapp-examples: $(WEBAPP_EXAMPLES)

$(WEBAPP_EXAMPLES):
	$(eval OUTPUTDIR=$(EXAMPLE_WEBAPP_OUTPUT)/$(dir $@))
	@mkdir -p ${OUTPUTDIR}
	@juvix compile -t wasm -r standalone $(EXAMPLEMILESTONE)/$@
	@cp $(dir $(EXAMPLEMILESTONE)/$@)* ${OUTPUTDIR}

# -- MDBook

docs/md/README.md : README.MNg
	@mkdir -p docs/md
	@${ORGTOMDPRG} README.org ${ORGOPTS}

docs/md/changelog.md : changelog.org
	@mkdir -p docs/md
	@${ORGTOMDPRG} changelog.org ${ORGOPTS}

docs/md/%.md : docs/org/%.org
	@echo "Processing ...  $@"
	@mkdir -p $(dir $@)
	${ORGTOMDPRG} $? ${ORGOPTS}

.PHONY: markdown-files
markdown-files: docs/md/README.md docs/md/changelog.md $(MDFILES)

.PHONY: markdown-docs
markdown-docs: markdown-files
	@echo "copying assets ..."
	@mkdir -p docs/md/assets/images
	@cp -v $(IMAGES) docs/md/assets/images/
	@mdbook build

.PHONY: serve-docs
serve-docs: markdown-files
	@mdbook serve --open

# -- Codebase Documentation

.PHONY : haddock
haddock :
	@cabal --docdir=docs/ --htmldir=docs/ haddock --enable-documentation

# ------------------------------------------------------------------------------
# -- Codebase Health
# ------------------------------------------------------------------------------

MAKEAUXFLAGS?=-s
MAKE=make ${MAKEAUXFLAGS}

ORMOLU?=stack exec -- ormolu
ORMOLUFILES = $(shell git ls-files '*.hs' '*.hs-boot' | grep -v '^contrib/')
ORMOLUFLAGS?=--no-cabal
ORMOLUMODE?=inplace

.PHONY: format
format: clang-format
	${ORMOLU} ${ORMOLUFLAGS} \
		--ghc-opt -XStandaloneDeriving \
		--ghc-opt -XUnicodeSyntax \
		--ghc-opt -XDerivingStrategies \
		--ghc-opt -XMultiParamTypeClasses  \
		--ghc-opt -XTemplateHaskell \
		--ghc-opt -XImportQualifiedPost \
			--mode ${ORMOLUMODE} \
		$(ORMOLUFILES)

.PHONY: clang-format
clang-format:
	@cd runtime && ${MAKE} format

.PHONY: check-ormolu
check-ormolu: export ORMOLUMODE = check
check-ormolu:
	@${MAKE} format

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

.PHONY: check
check: clean
	@${MAKE} build
	@${MAKE} install
	@${MAKE} test
	@${MAKE} smoke
	@${MAKE} format
	@${MAKE} pre-commit

# -- Build requirements

.PHONY: submodules
submodules:
	@git submodule sync
	@git submodule update --init --recursive

.PHONY: build
build: submodules runtime
	stack build ${STACKFLAGS}

.PHONY: fast-build
fast-build: submodules runtimestack
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
test: build
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

.PHONY : smoke
smoke: install
	@$(if $(SMOKE),, $(error "Smoke not found, please install it from https://github.com/SamirTalwar/smoke"))
	@smoke $(shell find tests -name '*.smoke.yaml')

# -- Release

.PHONY : changelog-updates
changelog-updates :
	@github_changelog_generator
	@pandoc CHANGELOG.md --from markdown --to org -o UPDATES-FOR-CHANGELOG.org

.PHONY : bench
bench: runtime submodules
	@stack bench
