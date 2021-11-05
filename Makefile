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
		then echo "Lines were found with more than 80 characters!"; \
		else echo "Succeed!"; \
		fi

.PHONY : hlint
hlint :
	hlint src

.PHONY : doc
doc :
	cabal haddock --enable-documentation

.PHONY : cabal-build
cabal-build :
	cabal build all

.PHONY : gen
 gen : 
	agda2hs ./src/MiniJuvix/Syntax/Core.agda -o src -XUnicodeSyntax -XStandaloneDeriving -XDerivingStrategies -XMultiParamTypeClasses
	agda2hs ./src/MiniJuvix/Syntax/Eval.agda -o src -XUnicodeSyntax -XStandaloneDeriving -XDerivingStrategies -XMultiParamTypeClasses

.PHONY : stan
stan :
	stan check --include --filter-all --directory=src 

setup:
	stack build --only-dependencies --jobs $(THREADS)

stack-build:
	stack build --fast --jobs $(THREADS)

stack-build-watch:
	stack build --fast --file-watch

repl-lib:
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
