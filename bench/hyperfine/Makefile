HELL := /bin/bash
HYPERFINE?=hyperfine
WARMUP?=2
RUNS?=10
HYPERFINEFLAGS?=--warmup ${WARMUP} \
				 --runs ${RUNS}

# Don't forget to add the comma at the end of the dev version
JUVIXVERSIONS?=-v0.4.3,-v0.5.0,-v0.5.1,-v0.5.2,-v0.5.3,-v0.5.4,-v0.5.5,
TASKS?="dev root" \
       "dev parse" \
	   "dev highlight" \
	   typecheck \
	   "compile -o /dev/null" \
	   "compile -o /dev/null -t wasm32-wasi" \
	   "compile -o /dev/null -t core" \
	   "compile -o /dev/null -t asm" \
	   eval

GLOBALOPTS?=

FILENAME?=fibo.juvix

all: hyperfine

.PHONY: hyperfine
hyperfine:
	@echo "# Hyperfine Benchmarks" > README.md
	@for task in ${TASKS}; do \
		TASK=$$task ${MAKE} run; \
	done;

.PHONY: run
run:
	@${HYPERFINE} ${HYPERFINEFLAGS} \
		--parameter-list version ${JUVIXVERSIONS} \
		'juvix{version} ${TASK} ${FILENAME} ${GLOBALOPTS}' \
		--export-markdown TMP.md
	@echo "" >> README.md
	@echo "## ${TASK}" >> README.md
	@echo "" >> README.md
	@cat TMP.md >> README.md
	rm -f TMP.md

.PHONY: clean
clean:
	@rm -rf *.json *.md
