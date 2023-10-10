INSTALL_DIR=/usr/local
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib
INSTALL_HEADER_DIR=$(INSTALL_DIR)/include
INSTALL_MAN_DIR=$(INSTALL_DIR)/share/man
INSTALL_STD_DIR=$(INSTALL_DIR)/share/kosu/std
DUNE=dune

OUTPUT=output
LIBNAME=libkosu.a

KOSU_RUNTIME_OBJ=$(OUTPUT)/u8.o $(OUTPUT)/s8.o

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
LINKER_OPTS=""
LINKER_ARGS=""
ifeq ($(OS_NAME), darwin)
	LINKER_OPTS="syslibroot \`xcrun --sdk macosx --show-sdk-path\`","lSystem"
else ifeq ($(OS_NAME), freebsd)
	LINKER_OPTS="lc","dynamic-linker /libexec/ld-elf.so.1","L/usr/lib"
	LINKER_ARGS="/lib64/crt1.o", "/lib64/crti.o", "/lib64/crtn.o"
else ifeq ($(OS_NAME), linux)
	LINKER_OPTS="lc","dynamic-linker /lib64/ld-linux-x86-64.so.2","L/usr/lib L/lib64"
	LINKER_ARGS="/usr/lib/crt1.o","/usr/lib/crti.o","/usr/lib/crtbegin.o","/usr/lib/crtend.o","/usr/lib/crtn.o"
endif

all: kosuc kosu kosu_runtime


kosuc:
	[ ! -d "$(OUTPUT)" ] && mkdir -p $(OUTPUT) || true
	$(DUNE) build lib/commandline/cliCommon/configure.exe
	make kosuConfig
	$(DUNE) build bin/$@.exe
	cp -f _build/default/bin/kosuc.exe $(OUTPUT)/$@

kosu:
	[ ! -d "$(OUTPUT)" ] && mkdir -p $(OUTPUT) || true
	$(DUNE) build lib/commandline/cliCommon/configure.exe
	make kosuConfig
	$(DUNE) build bin/$@.exe
	cp -f _build/default/bin/kosu.exe $(OUTPUT)/$@

kosuConfig:
	_build/default/lib/commandline/cliCommon/configure.exe -a $(OS_ARCH) -o $(OS_NAME) --cc $(OS_CC) --os-extension $(OS_DYNLIB_EXE) -b $(BRANCH) \
		--hash $(COMMIT_HASH) -H $(INSTALL_HEADER_DIR) -c $(INSTALL_STD_DIR) -r $(INSTALL_LIB_DIR) --lo $(LINKER_OPTS) --lar $(LINKER_ARGS)\
		> lib/commandline/cliCommon/kosuConfig.ml


kosu_runtime:
		[ ! -d "$(OUTPUT)" ] && mkdir -p $(OUTPUT) || true
		make $(LIBNAME)

$(LIBNAME): $(KOSU_RUNTIME_OBJ)
	$(OS_AR) rcs $(OUTPUT)/$(LIBNAME) $^


$(OUTPUT)/%.o: src/runtime/src/%.c
	$(OS_CC) -fPIC -O2 -c -o $@ $^

# TODO linker option

.PHONY: test

build:
	dune build

san:
	dune build --profile san

clean:
	[ -d "$(OUTPUT)" ] && rm -rf output || true
	dune clean

install:
	dune install

test:
	dune test
