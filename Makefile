
.PHONY: hello hello.S

hello: hello.S
	cc -o $@ $<

size: size.S
	cc -o $@ $<

size.S: size.kosu
	./kosuc $<

hello.S: hello.kosu
	./kosuc $<
	