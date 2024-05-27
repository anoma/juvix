
export

.PHONY: all
all: juvix_c juvix_rust

.PHONY: juvix_c
juvix_c:
	cd c && $(MAKE) -j 4 -s

.PHONY: juvix_rust
juvix_rust:
	cd rust && cargo build && cargo build --release

.PHONY: clean
clean:
	cd c && $(MAKE) clean

.PHONY: format
format:
	cd c && $(MAKE) format
