# This Makefile is provided for convenience.  This project is compiled using
# Dune, but course at Swarthmore typically provide projects which support
# "make".

.PHONY: build
build:
	dune build
	@[ -f _build/default/src/main/fb.bc ] && ( rm -f ./fb; ln -s _build/default/src/main/fb.bc ./fb )
	@[ -f _build/default/src/tests/tests.bc ] && ( rm -f ./tests; ln -s _build/default/src/tests/tests.bc ./tests )

fb: build

tests: build

.PHONY: clean
clean:
	dune clean
	rm -rf _build
	@rm -f ./fb
	@rm -f ./tests
