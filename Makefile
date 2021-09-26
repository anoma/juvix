
.PHONY : checklines
checklines :
	@grep '.\{81,\}' \
		--exclude-dir=src/fix-whitespace \
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

.PHONY : build
build :
	cabal build all

.PHONY : stan
stan :
	stan check --include --filter-all --directory=src 