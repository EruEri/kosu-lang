
.PHONY: hello hello.S

hello: hello.S
	cc -o $@ $<

hello.S: hello.kosu
	./kosuc $<
	