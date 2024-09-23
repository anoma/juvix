
export

.PHONY: all
all: juvix_c juvix_rust

# 2024-09-20: `-j` cannot be used in the juvix_c make invocation because it causes
# an error in the 'Build Linux static binary workflow'.
.PHONY: juvix_c
juvix_c:
	cd c && $(MAKE) -s

.PHONY: juvix_rust
juvix_rust:
	cd rust/juvix && cargo build && cargo build --release

.PHONY: clean
clean:
	cd c && $(MAKE) clean
	cd rust/juvix && cargo clean

.PHONY: format
format:
	cd c && $(MAKE) format
