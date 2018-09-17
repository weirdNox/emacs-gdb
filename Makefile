CC ?= gcc

COMMON_FLAGS = -Wall -Wextra -Wno-unused-parameter -DDEBUG -O0 -g3

all: multithread.out gdb-module.so
.PHONY: all

gdb-module.so: gdb-module.c emacs-module.h gdbwire.c
	$(CC) $(COMMON_FLAGS) -fPIC -shared $< -o $@

multithread.out: multithread.c
	$(CC) $(COMMON_FLAGS) -lpthread $< -o $@

clean:
	rm -f *.out *.so *.elc
.PHONY: clean
