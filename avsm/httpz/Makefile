.PHONY: all build test bench bench-compare bench-detail clean fmt erase-jane-syntax

all: build

build:
	dune build

test:
	dune runtest

# Benchmarks must run in the release profile. Dune's default dev profile
# passes -opaque for every module with an .mli, which disables flambda2
# cross-module inlining: the per-byte scanning primitives in Buf_read, Scan
# and Span then become indirect calls from Parser and Header_name, and parsing
# runs roughly twice as slow. Release numbers are the representative ones,
# and are also what `dune build -p` (and therefore opam) produces.
bench: bench-compare bench-detail

bench-compare:
	dune exec --profile release bench/bench_compare.exe

bench-detail:
	dune exec --profile release bench/bench_httpz.exe -- -quota 2

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
	@find core route eio_server test bench bin \
		-path '*/vendor/*' -prune -o \
		\( -name '*.ml' -o -name '*.mli' \) -print | \
		xargs -I {} ocamlformat --erase-jane-syntax -i {}
	@echo "Done. Files modified in place."
