all: release debug

release: wasm32-wasi native32

debug: wasm32-wasi-debug native32-debug

wasm32:
	$(MAKE) -f Makefile.generic CONFIG=WASM32+RELEASE

wasm32-wasi:
	$(MAKE) -f Makefile.generic CONFIG=WASM32-WASI+RELEASE

native32:
	$(MAKE) -f Makefile.generic CONFIG=NATIVE32+RELEASE

x86_32:
	$(MAKE) -f Makefile.generic CONFIG=X86_32+RELEASE

wasm32-debug:
	$(MAKE) -f Makefile.generic CONFIG=WASM32+DEBUG

wasm32-wasi-debug:
	$(MAKE) -f Makefile.generic CONFIG=WASM32-WASI+DEBUG

native32-debug:
	$(MAKE) -f Makefile.generic CONFIG=NATIVE32+DEBUG

x86_32-debug:
	$(MAKE) -f Makefile.generic CONFIG=X86_32+DEBUG

clean:
	@-rm -rf _build*

.PHONY: release debug wasm32 wasm32-wasi native32 x86_32 wasm32-debug wasm32-wasi-debug native32-debug x86_32-debug clean
