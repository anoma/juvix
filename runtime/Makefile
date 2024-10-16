
export

.PHONY: all
all: juvix_c juvix_rust

# 2024-09-20: `-j` cannot be used in the juvix_c make invocation because it causes
# an error in the 'Build Linux static binary workflow'.
.PHONY: juvix_c
juvix_c: juvix_c_native juvix_c_wasm
	cd c && $(MAKE) -s includes

.PHONY: juvix_c_native
juvix_c_native:
	cd c && $(MAKE) -s native64 native64-debug

.PHONY: juvix_c_wasm
juvix_c_wasm:
	if grep -q '"wasm": true' ../config/config.json; then cd c && $(MAKE) -s wasm32-wasi wasm32-wasi-debug; fi

.PHONY: juvix_rust
juvix_rust:
	if grep -q '"rust": true' ../config/config.json; then cd rust/juvix && cargo build && cargo build --release; fi

.PHONY: clean
clean:
	cd c && $(MAKE) clean
	cd rust/juvix && cargo clean

.PHONY: format
format:
	cd c && $(MAKE) format
