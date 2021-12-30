PWD=$(CURDIR)
PREFIX="$(PWD)/.stack-work/prefix"
UNAME := $(shell uname)

AGDA_FILES := $(wildcard ./src/MiniJuvix/Syntax/*.agda)
GEN_HS := $(patsubst %.agda, %.hs, $(AGDA_FILES))

ifeq ($(UNAME), Darwin)
	THREADS := $(shell sysctl -n hw.logicalcpu)
else ifeq ($(UNAME), Linux)
	THREADS := $(shell nproc)
else
	THREADS := $(shell echo %NUMBER_OF_PROCESSORS%)
endif

all:
	make prepare-push

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
	hlint src

.PHONY : haddock
haddock :
	cabal --docdir=docs/ --htmldir=docs/ haddock --enable-documentation 

.PHONY : docs
docs :
	cd docs ; \
	sh conv.sh

.PHONY : cabal
cabal :
	cabal build all

.PHONY : stan
stan :
	stan check --include --filter-all --directory=src 

setup:
	stack build --only-dependencies --jobs $(THREADS)

stack:
	stack build --fast --jobs $(THREADS)

stack-build-watch:
	stack build --fast --file-watch

repl:
	stack ghci MiniJuvix:lib

clean:
	cabal clean
	stack clean

clean-full:
	stack clean --full

format:
	find ./src/ -name "*.hs" -exec ormolu --mode inplace {} --ghc-opt -XStandaloneDeriving --ghc-opt -XUnicodeSyntax --ghc-opt -XDerivingStrategies --ghc-opt -XMultiParamTypeClasses \;

prepare-push:
	make checklines && make hlint && make format

.PHONY: install-agda
install-agda:
	git clone https://github.com/agda/agda.git
	cd agda
	cabal update
	cabal install --overwrite-policy=always --ghc-options='-O2 +RTS -M6G -RTS' alex-3.2.6
	cabal install --overwrite-policy=always --ghc-options='-O2 +RTS -M6G -RTS' happy-1.19.12
	pwd
	cabal install --overwrite-policy=always --ghc-options='-O2 +RTS -M6G -RTS' -foptimise-heavily

.PHONY : install-agda2hs
install-agda2hs:
	git clone https://github.com/agda/agda2hs.git
	cd agda2hs &&	cabal new-install --overwrite-policy=always
	mkdir -p .agda/
	touch .agda/libraries
	echo "agda2hs/agda2hs.agda-lib" > ~/.agda/libraries

.PHONY : agda
agda : 
	agda2hs ./src/MiniJuvix/Syntax/Core.agda -o src -XUnicodeSyntax -XStandaloneDeriving -XDerivingStrategies -XMultiParamTypeClasses
	agda2hs ./src/MiniJuvix/Syntax/Eval.agda -o src -XUnicodeSyntax -XStandaloneDeriving -XDerivingStrategies -XMultiParamTypeClasses