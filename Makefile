PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)
HLINTQUIET :=

ORGFILES = $(shell find docs/org -type f -name '*.org')
MDFILES:=$(patsubst docs/org/%,docs/md/%,$(ORGFILES:.org=.md))
ASSETS = seating-mascot.051c86a.svg Seating_Tara_smiling.svg teaching-mascot.f828959.svg

EXAMPLEMILESTONE=examples/milestone
EXAMPLEHTMLOUTPUT=_docs/examples/html
EXAMPLES=ValidityPredicates/SimpleFungibleToken.juvix \
		  MiniTicTacToe/MiniTicTacToe.juvix \
		  Fibonacci/Fibonacci.juvix \
		  Collatz/Collatz.juvix

ORGTOMDPRG ?=pandoc
ORGOPTS=--from org --to markdown_strict -s -o $@

ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

all:
	make pre-commit

docs/md/README.md :
	@mkdir -p docs/md
	@${ORGTOMDPRG} README.org ${ORGOPTS}

docs/md/%.md : docs/org/%.org
	@echo "Processing ...  $@"
	@mkdir -p $(dir $@)
	${ORGTOMDPRG} $? ${ORGOPTS}

.PHONY: markdown-docs
markdown-docs: docs/md/README.md $(MDFILES)
	@echo "copying assets ..."
	@mkdir -p docs/md/assets
	@cp -v $(addprefix assets/,$(ASSETS)) docs/md/assets
	mdbook build

.PHONY: serve-docs
serve-docs: $(MDFILES)
	mdbook serve --open

.PHONY : checklines
checklines :
	@grep '.\{81,\}' \
		--exclude=*.org \
		-l --recursive src; \
		status=$$?; \
		if [ $$status = 0 ] ; \
		then echo "Lines were found with more than 80 characters!" >&2 ; \
		else echo "Succeed!"; \
		fi

.PHONY : hlint
hlint :
	@hlint src app test ${HLINTQUIET}

.PHONY : haddock
haddock :
	cabal --docdir=docs/ --htmldir=docs/ haddock --enable-documentation

.PHONY : docs
docs :
	cd docs ; \
	sh conv.sh

.PHONY: ci
ci:
	make ci-build
	make install
	make ci-test
	make test-shell

.PHONY : build
build:
	stack build --fast --jobs $(THREADS)

.PHONY : ci-build
ci-build:
	stack build --fast --jobs $(THREADS) --pedantic

build-watch:
	stack build --fast --file-watch

.PHONY : cabal
cabal :
	cabal build all

clean:
	cabal clean
	stack clean

clean-full:
	stack clean --full
	rm -rf .hie
	rm -rf _docs

.PHONY : test
test:
	stack test --fast --jobs $(THREADS)

.PHONY : ci-test
ci-test:
	stack test --fast --jobs $(THREADS) --pedantic

.PHONY : test-skip-slow
test-skip-slow:
	stack test --fast --jobs $(THREADS) --ta '-p "! /slow tests/"'

.PHONY : test-watch
test-watch:
	stack test --fast --jobs $(THREADS) --file-watch

.PHONY : install-shelltest
install-shelltest:
	stack install shelltestrunner

.PHONY : test-shell
test-shell :
	shelltest --color --diff -a -j8 tests

format:
	@find . -name "*.hs" -exec ormolu --mode inplace {} --ghc-opt -XStandaloneDeriving --ghc-opt -XUnicodeSyntax --ghc-opt -XDerivingStrategies --ghc-opt -XMultiParamTypeClasses  --ghc-opt  -XTemplateHaskell --ghc-opt -XImportQualifiedPost \;

.PHONY: check-ormolu
check-ormolu:
	@find . -name "*.hs" -exec ormolu --mode check {} --ghc-opt -XStandaloneDeriving --ghc-opt -XUnicodeSyntax --ghc-opt -XDerivingStrategies --ghc-opt -XMultiParamTypeClasses  --ghc-opt  -XTemplateHaskell --ghc-opt -XImportQualifiedPost \;

.PHONY : install
install:
	stack install --fast --jobs $(THREADS)

.PHONY : install-watch
install-watch:
	stack install --fast --jobs $(THREADS) --file-watch

repl:
	stack ghci Juvix:lib

.PHONY: html-examples
html-examples: $(EXAMPLES)

$(EXAMPLES):
	$(eval OUTPUTDIR=$(EXAMPLEHTMLOUTPUT)/$(dir $@))
	mkdir -p ${OUTPUTDIR}
	juvix html $(EXAMPLEMILESTONE)/$@ --recursive --output-dir=./../../../${OUTPUTDIR} --print-metadata

.PHONY : install-pre-commit
install-pre-commit:
	pip install pre-commit
	pre-commit install

.PHONY : pre-commit
pre-commit :
	pre-commit run --all-files

.PHONY : update-submodules
update-submodules :
	git submodule foreach git pull origin main

.PHONY : juvix-stdlib
juvix-stdlib:
	git submodule update --init juvix-stdlib

.PHONY : get-changelog-updates
get-changelog-updates :
	@github_changelog_generator --since-tag $(shell git describe --tags $(shell git rev-list --tags --max-count=1)) 1> /dev/null
	pandoc CHANGELOG.md --from markdown --to org -o UPDATES-FOR-CHANGELOG.org
