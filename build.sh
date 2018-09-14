#!/usr/bin/env bash
cd "$( dirname "${BASH_SOURCE[0]}" )"

COMMON_FLAGS="-Wall -Wextra -Wno-unused-parameter"
DEBUG_FLAGS="-DDEBUG -O0 -g3"
RELEASE_FLAGS="-DNDEBUG -O3"
gcc module.c -o gdb-module.so -fPIC -shared $COMMON_FLAGS $DEBUG_FLAGS
