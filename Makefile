.PHONY: test

build:
	dune build

san:
	dune build --profile san

clean:
	dune clean

install:
	dune install

test:
	dune test