.PHONY: all build clean test install uninstall doc examples

benchmark:
	dune runtest ./lib_test
	OCAMLRUNPARAM=l=1000000000 dune exec -- ./benchmarks/simple_benchmark.exe

build:
	dune runtest ./lib_test

all: build

test:
	dune runtest

examples:
	dune build @examples

install:
	dune install

uninstall:
	dune uninstall

doc:
	dune build @doc

clean:
	rm -rf _build *.install
