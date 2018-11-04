DEBUG_FLAGS = -DDEBUG -O0 -g3
RELEASE_FLAGS = -O2

all: testbed.out gdb-module.so
.PHONY: all

gdb-module.so: gdb-module.c emacs-module.h gdbwire.c
	$(CC) -Wall -Wextra -Wno-unused-parameter $(RELEASE_FLAGS) -fPIC -shared $< -o $@

testbed.out: testbed.c
	$(CC) $(DEBUG_FLAGS) -lpthread $< -o $@

clean:
	rm -f *.out *.so *.elc
.PHONY: clean
