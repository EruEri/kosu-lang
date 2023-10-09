INSTALL_DIR=/usr/local
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib
INSTALL_HEADER_DIR=$(INSTALL_DIR)/include
INSTALL_MAN_DIR=$(INSTALL_DIR)/share/man
INSTALL_STD_DIR=$(INSTALL_DIR)/share/kosu/std

DUNE=dune

BRANCH=$(shell git rev-parse --abbrev-ref HEAD)
COMMIT_HASH=$(shell git describe --always --dirty --abbrev=7)

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