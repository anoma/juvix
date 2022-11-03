.PHONY: compile
compile:
	./compile.sh

.PHONY: clean
clean:
	-rm `find . -name '*.wasm'` 2>/dev/null || true
	-rm `find . -name '*.byte'` 2>/dev/null || true
	-rm `find . -name '*.exe'` 2>/dev/null || true
	-rm `find . -name '*.hi'` 2>/dev/null || true
	-rm `find . -name '*.o'` 2>/dev/null || true
	-rm `find . -name '*.cmo'` 2>/dev/null || true
	-rm `find . -name '*.cmi'` 2>/dev/null || true
	-rm `find . -name '*.cmx'` 2>/dev/null || true
	-rm -rf `find . -name '.juvix-build'` 2>/dev/null || true

.SILENT: clean
