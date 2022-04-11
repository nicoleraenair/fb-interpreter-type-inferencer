# This Makefile is provided for convenience.  This project is compiled using
# Dune, but course at Swarthmore typically provide projects which support
# "make".

.PHONY: build
build:
	dune build
	@[ -f _build/default/src/main/efb.bc ] && ( rm -f ./efb; ln -s _build/default/src/main/efb.bc ./efb )
	@[ -f _build/default/src/tests/tests.bc ] && ( rm -f ./tests; ln -s _build/default/src/tests/tests.bc ./tests )

fb: build

tests: build

.PHONY: clean
clean:
	dune clean
	rm -rf _build
	@rm -f ./efb
	@rm -f ./tests
