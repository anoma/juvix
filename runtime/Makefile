all: release debug

release: wasm32-wasi native64

debug: wasm32-wasi-debug native64-debug

wasm32:
	$(MAKE) -f Makefile.generic CONFIG=WASM32+RELEASE

wasm32-wasi:
	$(MAKE) -f Makefile.generic CONFIG=WASM32-WASI+RELEASE

native32:
	$(MAKE) -f Makefile.generic CONFIG=NATIVE32+RELEASE

native64:
	$(MAKE) -f Makefile.generic CONFIG=NATIVE64+RELEASE

x86_32:
	$(MAKE) -f Makefile.generic CONFIG=X86_32+RELEASE

wasm32-debug:
	$(MAKE) -f Makefile.generic CONFIG=WASM32+DEBUG

wasm32-wasi-debug:
	$(MAKE) -f Makefile.generic CONFIG=WASM32-WASI+DEBUG

native32-debug:
	$(MAKE) -f Makefile.generic CONFIG=NATIVE32+DEBUG

native64-debug:
	$(MAKE) -f Makefile.generic CONFIG=NATIVE64+DEBUG

x86_32-debug:
	$(MAKE) -f Makefile.generic CONFIG=X86_32+DEBUG

format:
	@clang-format -i `find src -name '*.c' -or -name '*.h'`

clean:
	@-rm -rf _build*

.PHONY: release debug format wasm32 wasm32-wasi native32 native64 x86_32 wasm32-debug wasm32-wasi-debug native32-debug native64-debug x86_32-debug clean
