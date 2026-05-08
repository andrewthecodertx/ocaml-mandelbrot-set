.PHONY: build run clean release deps

build:
	dune build

run:
	dune exec ./mandelbrot.exe

release:
	dune build --profile release

deps:
	opam install . --deps-only

clean:
	dune clean
