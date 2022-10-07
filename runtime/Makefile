all: wasm32 wasm32-wasi x86_32

wasm32:
	$(MAKE) -f Makefile.generic CONFIG=WASM32

wasm32-wasi:
	$(MAKE) -f Makefile.generic CONFIG=WASM32-WASI

x86_32:
	$(MAKE) -f Makefile.generic CONFIG=X86_32

clean:
	-rm -rf _build*

.PHONY: wasm32 wasm32-wasi x86_32 clean
