DOMAIN = bpf2riscv

$(DOMAIN)/compiler.c: $(DOMAIN)/make-compiler.rkt
	racket $(DOMAIN)/make-compiler.rkt

.PHONY: rerun test clean

rerun:
	racket $(DOMAIN)/make-compiler.rkt

test: test/tester.rkt
	racket test/tester.rkt

clean:
	rm -f */rwset-cache/*
	rm -rf */cchh/*
	rm -rf */compiled/*
