
export

.PHONY: all
all: juvix_c

.PHONY: juvix_c
juvix_c:
	cd c && $(MAKE) -j 4 -s

.PHONY: clean
clean:
	cd c && $(MAKE) clean

.PHONY: format
format:
	cd c && $(MAKE) format
