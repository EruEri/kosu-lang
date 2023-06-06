.PHONY: san clean

all:
	dune build

san:
	dune build --profile san

factoriel: test.san
	san cfg -dpng -icf factoriel test.san

clean:
	dune clean
