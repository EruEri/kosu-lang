
.PHONY: hello hello.S

hello: hello.S
	cc -o $@ $< -g

hello.S: hello.kosu
	./kosuc $<
	