PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)
HLINTQUIET :=


ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

all:
	make pre-commit

.PHONY : checklines
checklines :
	@grep '.\{81,\}' \
		--exclude=*.agda \
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

.PHONY : build
build:
	stack build --fast --jobs $(THREADS)

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

.PHONY : test
test:
	stack test --fast --jobs $(THREADS)

.PHONY : test-watch
test-watch:
	stack test --fast --jobs $(THREADS) --file-watch

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
	stack ghci MiniJuvix:lib

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

.PHONY : minijuvix-stdlib
minijuvix-stdlib:
	git submodule update --init minijuvix-stdlib
