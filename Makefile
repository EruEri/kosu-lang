.PHONY: test

build:
	dune build

san:
	dune build --profile san

clean:
	dune clean

test:
	dune test