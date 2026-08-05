.PHONY: all build test clean fmt erase-jane-syntax

all: build

build:
	dune build

test:
	dune runtest

clean:
	dune clean

fmt:
	dune fmt

# Erase Jane Street/OxCaml syntax from all OCaml files.
# Use this to maintain an upstream-compatible branch:
#   git checkout -b upstream
#   make erase-jane-syntax
#   git commit -am "Erase Jane Street syntax for upstream OCaml"
#
# Requires ocamlformat from the OxCaml opam repository.
erase-jane-syntax:
	@echo "Erasing Jane Street syntax from OCaml files..."
	@find lib test -name '*.ml' -o -name '*.mli' | \
		xargs -I {} ocamlformat --erase-jane-syntax -i {}
	@echo "Done. Files modified in place."
