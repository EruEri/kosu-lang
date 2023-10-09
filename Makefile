INSTALL_DIR=/usr/local
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib
INSTALL_HEADER_DIR=$(INSTALL_DIR)/include
INSTALL_MAN_DIR=$(INSTALL_DIR)/share/man
INSTALL_STD_DIR=$(INSTALL_DIR)/share/kosu/std

DUNE=dune

BRANCH=$(shell git rev-parse --abbrev-ref HEAD)
COMMIT_HASH=$(shell git describe --always --dirty --abbrev=7)
# lowercase name
OS_NAME=$(shell uname -s | tr A-Z a-z)
OS_ARCH=$(shell uname -m)
OS_DYNLIB_EXE=$(shell \
	if [ "$$(uname)" = "Darwin" ]; then \
		echo .dylib; \
	else \
		echo .so; \
	fi \
)

# TODO linker option

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