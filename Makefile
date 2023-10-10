INSTALL_DIR=/usr/local
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib
INSTALL_HEADER_DIR=$(INSTALL_DIR)/include
INSTALL_MAN_DIR=$(INSTALL_DIR)/share/man
INSTALL_STD_DIR=$(INSTALL_DIR)/share/kosu/std

DUNE=dune

OUTPUT=./output
LIBNAME=libkosu.a

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
OS_CC=$(shell which cc)
OS_AR=$(shell which ar)
KOSU_VERSION=$(cat kosu_lang.opam | grep ^version | awk '{print $2}')

all: kosuc kosu kosu_runtime


kosu_runtime:
		mkdir $(OUTPUT)
		make $(LIBNAME)

$(LIBNAME): $(OUTPUT)/u8.o
	$(OS_AR) rcs $(OUTPUT)/$(LIBNAME) $<


$(OUTPUT)/%.o: src/runtime/src/%.c
	$(OS_CC) -fPIC -O2 -c -o $@ $^

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
