PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)
HLINTQUIET :=

ASSETS = 	seating-mascot.051c86a.svg \
			Seating_Tara_smiling.svg \
			teaching-mascot.f828959.svg

ORGFILES = $(shell find docs/org -type f -name '*.org')
MDFILES:=$(patsubst docs/org/%,docs/md/%,$(ORGFILES:.org=.md))

EXAMPLEMILESTONE=examples/milestone
EXAMPLEHTMLOUTPUT=_docs/examples/html
EXAMPLES= 	HelloWorld/HelloWorld.juvix \
			Collatz/Collatz.juvix \
			Fibonacci/Fibonacci.juvix \
            Hanoi/Hanoi.juvix \
            PascalsTriangle/PascalsTriangle.juvix \
			TicTacToe/CLI/TicTacToe.juvix \
			ValidityPredicates/SimpleFungibleToken.juvix

EXAMPLE_WEBAPP_OUTPUT=_docs/examples/webapp
WEBAPP_EXAMPLES=TicTacToe/Web/TicTacToe.juvix

ORGTOMDPRG ?=pandoc
ORGOPTS=--from org --to markdown_strict -s -o $@

ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

all: install

clean: clean-runtime
	@stack clean --full
	@rm -rf .hie
	@rm -rf _docs
	@rm -rf docs/md

.PHONY: clean-runtime
clean-runtime:
	@cd runtime && make clean

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
	@juvix html $(EXAMPLEMILESTONE)/$@ --recursive --output-dir=$(CURDIR)/${OUTPUTDIR} --print-metadata

.PHONY: webapp-examples
webapp-examples: $(WEBAPP_EXAMPLES)

$(WEBAPP_EXAMPLES):
	$(eval OUTPUTDIR=$(EXAMPLE_WEBAPP_OUTPUT)/$(dir $@))
	@mkdir -p ${OUTPUTDIR}
	@juvix compile -t wasm -r standalone $(EXAMPLEMILESTONE)/$@
	@cp $(dir $(EXAMPLEMILESTONE)/$@)* ${OUTPUTDIR}

# -- MDBook

docs/md/README.md : README.org
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
	@mkdir -p docs/md/assets
	@cp -v $(addprefix assets/,$(ASSETS)) docs/md/assets
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

ORMOLUFILES = $(shell git ls-files '*.hs' '*.hs-boot' | grep -v '^contrib/')
ORMOLUFLAGS?=--no-cabal
ORMOLUMODE?=inplace

.PHONY: format
format: clang-format
	@stack exec -- ormolu ${ORMOLUFLAGS} \
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
	@cd runtime && make format

.PHONY: check-ormolu
check-ormolu: export ORMOLUMODE = check
check-ormolu:
	make format

.PHONY : hlint
hlint :
	@hlint src app test ${HLINTQUIET}

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

.PHONY: check
check:
	@make build
	@make install
	@make test
	@make test-shell
	@make format
	@make pre-commit

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
	cd runtime && make -j 4

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
	@stack test ${STACKFLAGS}

.PHONY : fast-test
fast-test: fast-build
	@stack test --fast ${STACKFLAGS}

.PHONY : test-skip-slow
test-skip-slow:
	@stack test ${STACKFLAGS} --ta '-p "! /slow tests/"'

.PHONY : fast-test-skip-slow
fast-test-skip-slow:
	@stack test --fast ${STACKFLAGS} --ta '-p "! /slow tests/"'

SHELLTEST := $(shell command -v shelltest 2> /dev/null)

.PHONY : test-shell
test-shell : install
	@$(if $(SHELLTEST),, stack install shelltestrunner)
	shelltest --color --diff -a tests

# -- Release

.PHONY : changelog-updates
changelog-updates :
	@github_changelog_generator
	@pandoc CHANGELOG.md --from markdown --to org -o UPDATES-FOR-CHANGELOG.org
